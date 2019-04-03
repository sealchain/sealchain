{-# LANGUAGE RecordWildCards #-}

-- | Functions operating on UTXO (in 'VerifyAndApplyM' monad).

module Seal.Chain.Txp.Toil.Verify
       ( VTxContext (..)
       , VerifyTxUtxoRes (..)
       , verifyTx
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Serokell.Util (allDistinct, enumerate)

import           Seal.Chain.Txp.Toil.Failure (ToilVerFailure (..),
                     TxOutVerFailure (..), WitnessVerFailure (..))
import           Seal.Chain.Txp.Toil.Monad (VerifyAndApplyM, utxoGet)
import           Seal.Chain.Txp.Toil.Types (TxFee (..))
import           Seal.Chain.Txp.Toil.VerifySum (verifySum, runVerifySumM)
import           Seal.Chain.Txp.TxAttributes (TxAttributes)
import           Seal.Chain.Txp.Tx (Tx (..), TxIn (..), TxOut (..), AccountIn (..),
                     AccountOut (..), isTxInUnknown, isSealTxOut)
import           Seal.Chain.Txp.TxAux (TxAux (..))
import           Seal.Chain.Txp.TxOutAux (TxOutAux (..))
import           Seal.Chain.Txp.TxWitness (TxInWitness (..), TxSigData (..))
import           Seal.Chain.Txp.Undo (TxUndo)
import           Seal.Core (AddrType (..), Address (..), Account (..),
                     isRedeemAddress, isUnknownAddressType)
import           Seal.Core.Attributes (Attributes (..), areAttributesKnown)
import           Seal.Core.Common (AddrAttributes (..), AddrCategory (..), 
                     checkPubKeyAddress, checkRedeemAddress)
import           Seal.Core.Slotting (SlotId)
import           Seal.Crypto (SignTag (SignRedeemTx, SignTx),
                     checkSig, hash, redeemCheckSig)
import           Seal.Crypto.Configuration (ProtocolMagic)
import           Seal.Util (liftEither)

----------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------

-- | Global context data needed for tx verification. VT stands for
-- "Verify Tx". To be populated in further versions.
data VTxContext = VTxContext
    { -- | Verify addresses' and
      -- witnesses' types are known, attributes are known too.
      vtcVerifyAllIsKnown :: !Bool
    , vtcSlotId           :: !SlotId         -- ^ Slot id of block transaction is checked in
--    , vtcLeaderId :: !StakeholderId  -- ^ Leader id of block transaction is checked in
    }

-- | Result of successful 'Tx' verification based on Utxo.
data VerifyTxUtxoRes = VerifyTxUtxoRes
    { vturUndo :: !TxUndo
    -- ^ 'TxUndo' for the verified transaction.
    , vturFee  :: !(Maybe TxFee)
    -- ^ Fee of the verified transaction. Can be 'Nothing' if there
    -- are inputs of unknown types.
    } deriving (Show)

-- | CHECK: Verify Tx correctness using 'UtxoLookup'.
-- Specifically there are the following checks:
--
-- * every input is a known unspent output;
-- * sum of inputs >= sum of outputs;
-- * every input has a proper witness verifying that input;
--
verifyTx
    :: forall m.(Monad m)
    => ProtocolMagic
    -> VTxContext
    -> TxAux
    -> ExceptT ToilVerFailure (VerifyAndApplyM m) VerifyTxUtxoRes
verifyTx protocolMagic ctx@VTxContext {..} ta@(TxAux UnsafeTx {..} _) = do
    let unknownTxInMB = find (isTxInUnknown . snd) $ zip [0..] _txUtxoInputs
    case (vtcVerifyAllIsKnown, unknownTxInMB) of
        (True, Just (inpId, txIn)) -> throwError $
            ToilUnknownInput inpId txIn
        (False, Just _) -> do
            -- Case when at least one input isn't known
            minimalReasonableChecks
            resolvedUtxoInputs :: [Maybe (TxIn, TxOutAux)] <-
                mapM
                    (lift . fmap rightToMaybe . runExceptT . resolveUtxoInput)
                    _txUtxoInputs

            liftEither $ verifyLockedUtxoInputs (catMaybes resolvedUtxoInputs) vtcSlotId
            pure VerifyTxUtxoRes
                 { vturUndo = map (fmap snd) resolvedUtxoInputs
                 , vturFee = Nothing
                 }
        (_, Nothing) -> do
            -- Case when all inputs are known
            minimalReasonableChecks
            resolvedUtxoInputs <- mapM resolveUtxoInput _txUtxoInputs
            liftEither $ do
                verifyLockedUtxoInputs resolvedUtxoInputs vtcSlotId
                txFee <- runVerifySumM resolvedUtxoInputs _txUtxoOutputs _txAccountInputs _txAccountOutputs verifySum
                verifyKnownInputs protocolMagic ctx resolvedUtxoInputs ta
                when vtcVerifyAllIsKnown $ verifyAttributesAreKnown _txAttributes
                pure VerifyTxUtxoRes
                    { vturUndo = map (Just . snd) resolvedUtxoInputs
                    , vturFee = Just txFee
                    }
  where
    minimalReasonableChecks :: ExceptT ToilVerFailure (VerifyAndApplyM m) ()
    minimalReasonableChecks = liftEither $ do
        verifyOutputs ctx ta

-- | For a given TxIn, look up the TxOutAux that it is spending.
resolveUtxoInput 
    :: (Monad m) 
    => TxIn 
    -> ExceptT ToilVerFailure (VerifyAndApplyM m) (TxIn, TxOutAux)
resolveUtxoInput txIn =
    (txIn, ) <$> (note (ToilNotUnspent txIn) =<< lift (utxoGet txIn))

verifyLockedUtxoInputs :: [(TxIn, TxOutAux)] -> SlotId -> Either ToilVerFailure ()
verifyLockedUtxoInputs resolvedUtxoInputs slot = do
    let sealInputs = filter (isSealTxOut . toaOut . snd) resolvedUtxoInputs
    forM_ sealInputs $ \(txIn, TxOutAux txOutSeal) ->
        case (txOutLockTime txOutSeal) of
            Nothing       -> pure () 
            Just lockTime -> if | (slot < lockTime) -> throwError $ ToilLockedInput txIn txOutSeal
                                | otherwise         -> pure ()

verifyOutputs :: VTxContext -> TxAux -> Either ToilVerFailure ()
verifyOutputs VTxContext {..} (TxAux UnsafeTx {..} _) = do
    mapM_ verifyUtxoOutput . enumerate $ _txUtxoOutputs
    mapM_ verifyAcctOutput . enumerate $ _txAccountOutputs
  where
    verifyUtxoOutput :: (Word32, TxOut) -> Either ToilVerFailure ()
    verifyUtxoOutput (i, txOut) = do
        let addr@(Address{..}) = txOutAddress txOut
        when (vtcVerifyAllIsKnown && not (areAttributesKnown addrAttributes)) $
            throwError $ ToilInvalidOutput i (TxOutUnknownAttributes addr)
        when (vtcVerifyAllIsKnown && isUnknownAddressType addr) $
            throwError $ ToilInvalidOutput i (TxOutUnknownAddressType addr)
        when (isRedeemAddress addr) $
            throwError $ ToilInvalidOutput i (TxOutRedeemAddressProhibited addr)

    verifyAcctOutput :: (Word32, AccountOut) -> Either ToilVerFailure ()
    verifyAcctOutput (i, DepositOutput Account{..} _) = do
        let addr@(Address{..}) = getAccount
        when (vtcVerifyAllIsKnown && not (areAttributesKnown addrAttributes)) $
            throwError $ ToilInvalidOutput i (TxOutUnknownAttributes addr)
        when (addrType /= ATPubKey) $
            throwError $ ToilInvalidOutput i (TxOutAccountAddressType addr)
        when ((aaCategory $ attrData $ addrAttributes) /= (Just ACUserAccount)) $
            throwError $ ToilInvalidOutput i (TxOutAccountAddrCategory addr)

-- Verify inputs of a transaction after they have been resolved
-- (implies that they are known).
verifyKnownInputs ::
       ProtocolMagic
    -> VTxContext
    -> [(TxIn, TxOutAux)]
    -> TxAux
    -> Either ToilVerFailure ()
verifyKnownInputs protocolMagic VTxContext {..} resolvedUtxoInputs (TxAux taTx@UnsafeTx{..} taWitness) = do
    unless allUtxoInputsDifferent $ throwError ToilRepeatedInput
    unless allAcctInputsDifferent $ throwError ToilRepeatedInput
    mapM_ (uncurry3 checkUtxoInput) $
        zip3 [0 ..] resolvedUtxoInputs resolvedUtxoWitness
    mapM_ (uncurry3 checkAccountInput) $
        zip3 [0 ..] _txAccountInputs resolvedAcctWitness
    mapM_ (uncurry checkUnknownWitness) $
        zip [0..] remainAcctWitness
  where
    uncurry3 f (a, b, c) = f a b c
    txHash = hash taTx
    txSigData = TxSigData txHash
    (resolvedUtxoWitness, acctWitness) = splitAt (length resolvedUtxoInputs) (toList taWitness)
    (resolvedAcctWitness, remainAcctWitness) = splitAt (length _txAccountInputs) acctWitness

    allUtxoInputsDifferent :: Bool
    allUtxoInputsDifferent = allDistinct (map fst resolvedUtxoInputs)

    allAcctInputsDifferent :: Bool
    allAcctInputsDifferent = allDistinct (map riAccount _txAccountInputs)

    checkUtxoInput
        :: Word32           -- ^ Input index
        -> (TxIn, TxOutAux) -- ^ Input and corresponding output data
        -> TxInWitness
        -> Either ToilVerFailure ()
    checkUtxoInput i (txIn, TxOutAux{..}) witness = do
        unless (checkSpendingData (txOutAddress toaOut) witness) $
            throwError $ ToilWitnessDoesntMatchUtxo i txIn (txOutAddress toaOut) witness
        whenLeft (checkWitness witness) $ \err ->
            throwError $ ToilInvalidWitness i witness err

    checkAccountInput
        :: Word32           -- ^ Input index
        -> AccountIn
        -> TxInWitness
        -> Either ToilVerFailure ()
    checkAccountInput i acctIn@(RedeemInput acct@Account{..} _ _) witness = do
        unless (isPkWitness witness) $
            throwError ToilUnexpectedWitnessType
        unless (checkSpendingData getAccount witness) $
            throwError $ ToilWitnessDoesntMatchAcc i acctIn acct witness
        whenLeft (checkWitness witness) $ \err ->
            throwError $ ToilInvalidWitness i witness err
    
    checkUnknownWitness 
        :: Word32 
        -> TxInWitness
        -> Either ToilVerFailure ()
    checkUnknownWitness i witness = do
        unless (isPkWitness witness) $
            throwError ToilUnexpectedWitnessType
        whenLeft (checkWitness witness) $ \err ->
            throwError $ ToilInvalidWitness i witness err

    isPkWitness :: TxInWitness -> Bool
    isPkWitness (PkWitness _ _) = True
    isPkWitness _               = False

    checkSpendingData addr wit = case wit of
        PkWitness twKey _            -> checkPubKeyAddress twKey addr
        RedeemWitness twRedeemKey _  -> checkRedeemAddress twRedeemKey addr
        UnknownWitnessType witTag _  -> case addrType addr of
            ATUnknown addrTag -> addrTag == witTag
            _                 -> False

    checkWitness :: TxInWitness -> Either WitnessVerFailure ()
    checkWitness witness = case witness of
        PkWitness twKey twSig ->
            unless (checkSig protocolMagic SignTx twKey txSigData twSig) $
                throwError WitnessWrongSignature
        RedeemWitness twRedeemKey twRedeemSig ->
            unless (redeemCheckSig protocolMagic SignRedeemTx twRedeemKey txSigData twRedeemSig) $
                throwError WitnessWrongSignature
        UnknownWitnessType t _ ->
            when vtcVerifyAllIsKnown $
                throwError $ WitnessUnknownType t

verifyAttributesAreKnown
    :: TxAttributes -> Either ToilVerFailure ()
verifyAttributesAreKnown attrs =
    unless (areAttributesKnown attrs) $
    throwError $ ToilUnknownAttributes (attrRemain attrs)
