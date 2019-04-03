{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | All high-level logic of Toil.  It operates in 'LocalToilM' and
-- 'GlobalToilM'.

module Seal.Chain.Txp.Toil.Logic
       ( verifyToil
       , applyToil
       , rollbackToil

       , normalizeToil
       , processTx

       , applyTxToUtxo
       , rollbackTxUtxo
       ) where

import           Universum

import           Control.Lens ((.=))
import           Control.Monad.Except (ExceptT, MonadError (throwError),
                     throwError, mapExceptT)
import           Serokell.Data.Memory.Units (Byte)

import           Seal.Binary.Class (biSize, decodeFull', serialize')
import           Seal.Chain.Contract (ApplyState (..), ApplyEnv (..), 
                     runApply, applyCommands)
import           Seal.Chain.Genesis (GenesisWStakeholders)
import           Seal.Chain.Txp.Configuration (TxpConfiguration (..),
                     memPoolLimitTx)
import           Seal.Chain.Txp.Toil.Failure (ToilVerFailure (..))
import           Seal.Chain.Txp.Toil.Monad (GlobalToilM, LocalToilM,
                     VerifyAndApplyM,
                     utxoDel, utxoPut,
                     hasTx, memPoolSize, putTxWithUndo,
                     verifyAndApplyMToLocalToilM,
                     vaaeAcctMPDB, vaaePactMPDB,
                     vaasAcctModifier, vaasPactModifier,
                     verifyAndApplyMToGlobalToilM)
import           Seal.Chain.Txp.Toil.Stakes (applyTxsToStakes, rollbackTxsStakes)
import           Seal.Chain.Txp.Toil.Types (TxFee (..))
import           Seal.Chain.Txp.Toil.Verify (VerifyTxUtxoRes (..), VTxContext (..), 
                      verifyTx)
import           Seal.Chain.Txp.Topsort (topsortTxs)
import           Seal.Chain.Txp.Tx (Tx (..), TxId, TxOut (..), TxIn (..),
                     AccountIn (..), AccountOut (..),
                     isTxInUnknown, txOutAddress)
import           Seal.Chain.Txp.TxAux (TxAux (..), checkTxAux)
import           Seal.Chain.Txp.TxOutAux (TxOutAux (..), toaOut)
import           Seal.Chain.Txp.TxWitness (TxInWitness (..))
import           Seal.Chain.Txp.Undo (TxUndo, TxpUndo)
import           Seal.Chain.Update.BlockVersionData (BlockVersionData (..),
                     isBootstrapEraBVD)
import           Seal.Core (AddrAttributes (..), AddrStakeDistribution (..),
                     Address, EpochIndex, addrAttributesUnwrapped,
                     isRedeemAddress)
import           Seal.Core.Common (Currency (integerToMoney), AccountState (..),
                     Cmd (..), fromPublicKey,
                     unsafeAddCoinGroup, unsafeSubCoinGroup, anyGreatThanCoinGroup)
import qualified Seal.Core.Common as Fee (TxFeePolicy (..),
                     calculateTxSizeLinear)
import           Seal.Crypto (ProtocolMagic, WithHash (..), hash)
import           Seal.Core.Slotting (SlotId (..))
import           Seal.Mpt.MerklePatricia (getKeyVal)
import           Seal.Mpt.MerklePatricia.Utils (bytesToNibbleString)
import           Seal.Util (liftEither)
import qualified Seal.Util.Modifier as MM

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- CHECK: @verifyToil
-- | Verify transactions correctness with respect to Utxo applying
-- them one-by-one.
-- Note: transactions must be topsorted to pass check.
-- Warning: this function may apply some transactions and fail
-- eventually.
--
-- If the 'Bool' argument is 'True', all data (script versions,
-- witnesses, addresses, attributes) must be known. Otherwise unknown
-- data is just ignored.
verifyToil 
    :: (MonadIO m)
    => ProtocolMagic
    -> BlockVersionData
    -> SlotId
    -> Bool
    -> [TxAux]
    -> ExceptT ToilVerFailure (GlobalToilM m) TxpUndo
verifyToil pm bvd curSlot verifyAllIsKnown =
    mapM verifySingle
  where 
    verifySingle tx = 
        mapExceptT verifyAndApplyMToGlobalToilM $
            verifyAndApplyTx pm bvd curSlot verifyAllIsKnown $ (hash (taTx tx), tx)

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
applyToil :: (MonadIO m) => GenesisWStakeholders -> [(TxAux, TxUndo)] -> GlobalToilM m ()
applyToil _ [] = pass
applyToil bootStakeholders txun = do
    applyTxsToStakes bootStakeholders txun
    mapM_ applySingle txun
  where
    applyTx (ta@TxAux{..}, _) = do
        let txId = hash taTx
        lift $ applyTxToUtxo $ WithHash taTx txId
        applyTxToAccountAndPact (txId, ta)

    applySingle = verifyAndApplyMToGlobalToilM . runExceptT . applyTx


-- | Rollback transactions from one block.
rollbackToil :: Monad m => GenesisWStakeholders -> [(TxAux, TxUndo)] -> GlobalToilM m ()
rollbackToil bootStakeholders txun = do
    rollbackTxsStakes bootStakeholders txun
    verifyAndApplyMToGlobalToilM $
        mapM_ rollbackTxUtxo $ reverse txun
    -- only rollback utxo

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
processTx
    :: (MonadIO m) 
    => ProtocolMagic
    -> TxpConfiguration
    -> BlockVersionData
    -> SlotId
    -> (TxId, TxAux)
    -> ExceptT ToilVerFailure (LocalToilM m) TxUndo
processTx pm txpConfig bvd curSlot tx@(txId, aux) = do
    whenM (lift $ hasTx txId) $ throwError ToilKnown
    whenM ((>= memPoolLimitTx txpConfig) <$> lift memPoolSize) $
        throwError (ToilOverwhelmed $ memPoolLimitTx txpConfig)
    undo <- mapExceptT verifyAndApplyMToLocalToilM $ verifyAndApplyTx pm bvd curSlot True tx
    lift $ putTxWithUndo txId aux undo
    return undo

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
normalizeToil
    :: forall m.(MonadIO m) 
    => ProtocolMagic
    -> TxpConfiguration
    -> BlockVersionData
    -> SlotId
    -> [(TxId, TxAux)]
    -> LocalToilM m ()
normalizeToil pm txpConfig bvd curSlot txs = mapM_ normalize ordered
  where
    -- If there is a cycle in the tx list, topsortTxs returns Nothing.
    -- Why is that not an error? And if its not an error, why bother
    -- top-sorting them anyway?
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, txAux) = WithHash (taTx txAux) i
    normalize ::
           (TxId, TxAux)
        -> LocalToilM m ()
    normalize = void . runExceptT . processTx pm txpConfig bvd curSlot

isRedeemTx :: TxUndo -> Bool
isRedeemTx resolvedOuts = all isRedeemAddress inputAddresses
  where
    inputAddresses =
        fmap (txOutAddress . toaOut) . catMaybes . toList $ resolvedOuts

verifyGState ::
       BlockVersionData
    -> EpochIndex
    -> TxAux
    -> VerifyTxUtxoRes
    -> Either ToilVerFailure ()
verifyGState bvd@BlockVersionData {..} curEpoch txAux vtur = do
    verifyBootEra bvd curEpoch txAux
    let txFeeMB = vturFee vtur
    let txSize = biSize txAux
    let limit = bvdMaxTxSize
    unless (isRedeemTx $ vturUndo vtur) $ whenJust txFeeMB $ \txFee ->
        verifyTxFeePolicy txFee bvdTxFeePolicy txSize
    when (txSize > limit) $
        throwError $ ToilTooLargeTx txSize limit

verifyBootEra ::
       BlockVersionData -> EpochIndex -> TxAux -> Either ToilVerFailure ()
verifyBootEra bvd curEpoch TxAux {..} = do
    when (isBootstrapEraBVD bvd curEpoch) $
        whenNotNull notBootstrapDistrAddresses $
        throwError . ToilNonBootstrapDistr
  where
    notBootstrapDistrAddresses :: [Address]
    notBootstrapDistrAddresses =
        filter (not . isBootstrapEraDistr) $
        map txOutAddress $ _txUtxoOutputs taTx
    isBootstrapEraDistr :: Address -> Bool
    isBootstrapEraDistr (addrAttributesUnwrapped -> AddrAttributes {..}) =
        case aaStakeDistribution of
            BootstrapEraDistr -> True
            _                 -> False

verifyTxFeePolicy ::
       TxFee -> Fee.TxFeePolicy -> Byte -> Either ToilVerFailure ()
verifyTxFeePolicy (TxFee txFee) policy txSize = case policy of
    Fee.TxFeePolicyTxSizeLinear txSizeLinear -> do
        let
            -- We use 'ceiling' to convert from a fixed-precision fractional
            -- to coin amount. The actual fee is always a non-negative integer
            -- amount of coins, so if @min_fee <= fee@ holds (the ideal check),
            -- then @ceiling min_fee <= fee@ holds too.
            -- The reason we can't compare fractionals directly is that the
            -- minimal fee may need to appear in an error message (as a reason
            -- for rejecting the transaction).
            mTxMinFee = integerToMoney . ceiling $
                Fee.calculateTxSizeLinear txSizeLinear txSize
        -- The policy must be designed in a way that makes this impossible,
        -- but in case the result of its evaluation is negative or exceeds
        -- maximum coin value, we throw an error.
        txMinFee <- case mTxMinFee of
            Left reason -> throwError $
                ToilInvalidMinFee policy reason txSize
            Right a -> return a
        unless (txMinFee <= txFee) $
            throwError $
                ToilInsufficientFee policy (TxFee txFee) (TxFee txMinFee) txSize
    Fee.TxFeePolicyUnknown _ _ ->
        -- The minimal transaction fee policy exists, but the current
        -- version of the node doesn't know how to handle it. There are
        -- three possible options mentioned in [CSLREQ-157]:
        -- 1. Reject all new-coming transactions (b/c we can't calculate
        --    fee for them)
        -- 2. Use latest policy of known type
        -- 3. Discard the check
        -- Implementation-wise, the 1st option corresponds to throwing an
        -- error here (reject), the 3rd option -- doing nothing (accept), and
        -- the 2nd option would require some engineering feats to
        -- retrieve previous 'TxFeePolicy' and check against it.
        return ()

----------------------------------------------------------------------------
-- Verify and Apply logic
----------------------------------------------------------------------------

-- | Remove unspent outputs used in given transaction, add new unspent
-- outputs.
applyTxToUtxo :: (Monad m) => WithHash Tx -> (VerifyAndApplyM m) ()
applyTxToUtxo (WithHash UnsafeTx {..} txid) = do
    mapM_ utxoDel $ filter (not . isTxInUnknown) _txUtxoInputs
    mapM_ applyOutput . zip [0 ..] $ map TxOutAux _txUtxoOutputs
  where
    applyOutput (idx, toa) = utxoPut (TxInUtxo txid idx) toa

-- | Rollback application of given transaction to Utxo using Undo
-- data.  This function assumes that transaction has been really
-- applied and doesn't check anything.
rollbackTxUtxo :: (Monad m) => (TxAux, TxUndo) -> (VerifyAndApplyM m) ()
rollbackTxUtxo (txAux, undo) = do
    let tx@UnsafeTx {..} = taTx txAux
    let txid = hash tx
    mapM_ utxoDel $ take (length _txUtxoOutputs) $ map (TxInUtxo txid) [0..]
    mapM_ (uncurry utxoPut) $ mapMaybe knownInputAndUndo $ zip _txUtxoInputs undo
  where
    knownInputAndUndo (_,         Nothing) = Nothing
    knownInputAndUndo (TxInUnknown _ _, _) = Nothing
    knownInputAndUndo (inp, Just u)        = Just (inp, u)

-- Note: it doesn't consider/affect stakes! That's because we don't
-- care about stakes for local txp.
verifyAndApplyTx 
    :: (MonadIO m) 
    => ProtocolMagic
    -> BlockVersionData
    -> SlotId
    -> Bool
    -> (TxId, TxAux)
    -> ExceptT ToilVerFailure (VerifyAndApplyM m) TxUndo
verifyAndApplyTx pm adoptedBVD curSlot verifyVersions tx@(txId, txAux) = do
    whenLeft (checkTxAux txAux) (throwError . ToilInconsistentTxAux)
    let ctx = VTxContext verifyVersions curSlot
    vtur@VerifyTxUtxoRes {..} <- verifyTx pm ctx txAux
    liftEither $ verifyGState adoptedBVD (siEpoch curSlot) txAux vtur
    lift $ applyTxToUtxo $ WithHash (taTx txAux) txId
    applyTxToAccountAndPact tx
    return vturUndo


applyTxToAccountAndPact 
    :: (MonadIO m) 
    => (TxId, TxAux)
    -> ExceptT ToilVerFailure (VerifyAndApplyM m) ()
applyTxToAccountAndPact tx = do
    applyTxToAccount tx
    applyTxToPact tx

-- | update account state in give transaction
applyTxToAccount 
    :: (MonadIO m) 
    => (TxId, TxAux)
    -> ExceptT ToilVerFailure (VerifyAndApplyM m) ()
applyTxToAccount (_, TxAux UnsafeTx{..} _) = do
    mapM_ applyAccountInput _txAccountInputs
    mapM_ applyAccountOutput _txAccountOutputs
  where
      acctToKey = bytesToNibbleString . serialize'
      lookupAccount acct = do
          acctMPDB <- view vaaeAcctMPDB
          mvalue <- getKeyVal acctMPDB (acctToKey acct)
          return $ mvalue >>= either (const Nothing) Just . decodeFull'

      applyAccountInput ri@RedeemInput{..}= do
          acctModifier <- use vaasAcctModifier
          mstate <- MM.lookupM lookupAccount riAccount acctModifier
          case mstate of
              Just st@AccountState{..} -> do
                  when (riValue `anyGreatThanCoinGroup` balance) $
                      throwError $ ToilInvalidAccountIn ri (Just st)
                  when (riNonce /= nonce + 1) $
                      throwError $ ToilInvalidAccountIn ri (Just st)
                  let newst = st{ balance = unsafeSubCoinGroup balance riValue
                                , nonce = nonce + 1
                                }
                  vaasAcctModifier .= MM.insert riAccount newst acctModifier
              Nothing -> throwError $ ToilInvalidAccountIn ri Nothing

      applyAccountOutput DepositOutput{..}= do
          acctModifier <- use vaasAcctModifier
          mstate <- MM.lookupM lookupAccount doAccount acctModifier
          let newst = case mstate of
                           Just st@AccountState{balance = balance} ->
                               st{balance = unsafeAddCoinGroup balance doValue}
                           Nothing -> AccountState 0 doValue
          vaasAcctModifier .= MM.insert doAccount newst acctModifier

applyTxToPact 
    :: (MonadIO m) 
    => (TxId, TxAux)
    -> ExceptT ToilVerFailure (VerifyAndApplyM m) ()
applyTxToPact (txId, TxAux (UnsafeTx {..}) taWitness) 
    | null _txCmds = return ()
    | otherwise     = do
        let knownSigners = (map riAccount _txAccountInputs)
            knownUtxoInputs = filter (not . isTxInUnknown) _txUtxoInputs
            (_, acctWitness) = splitAt (length knownUtxoInputs) (toList taWitness)
            (_, remainAcctWitness) = splitAt (length _txAccountInputs) acctWitness -- | All of remainAcctWitness are PKWitness
            additionalSigners = map (\(PkWitness pubKey _) -> fromPublicKey pubKey) remainAcctWitness
            allSigners = knownSigners <> additionalSigners
            cmdStrings = map getCmd _txCmds

        applyState <- ApplyState <$> (use vaasAcctModifier) <*> (use vaasPactModifier)
        applyEnv <- ApplyEnv <$> (view vaaeAcctMPDB) <*> (view vaaePactMPDB)

        (err, applyState') <- liftIO $ runApply applyState applyEnv (applyCommands txId cmdStrings allSigners)
        case err of 
            Left msg -> throwError $ ToilPactEvalFailure msg
            Right _  -> do
                vaasAcctModifier .= _asAcctModifier applyState'
                vaasPactModifier .= _asPactModifier applyState'
                return ()
