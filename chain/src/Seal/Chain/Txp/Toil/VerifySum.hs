{-# LANGUAGE RecordWildCards #-}

module Seal.Chain.Txp.Toil.VerifySum
    ( runVerifySumM
    , verifySum
    ) where

import           Universum

import           Control.Monad.Except (Except, runExcept, throwError)

import           Seal.Core.Common (CoinGroup(..), Currency (..))
import           Seal.Chain.Txp.Tx (TxIn (..), TxOut (..),
                     AccountIn (..), AccountOut (..), IssueState(..), IssueCert(..), 
                     isStateTxOut, isGoldTxOut, isDollarTxOut, isSealTxOut)
import           Seal.Chain.Txp.TxOutAux (TxOutAux (..))
import           Seal.Chain.Txp.Toil.Failure (ToilVerFailure (..))
import           Seal.Chain.Txp.Toil.Types (TxFee (..))

data VerifyState 
   = VerifyState 
   { vsUtxoInputs ::  ![(TxIn, TxOutAux)]
   , vsUtxoOutputs :: ![TxOut]
   , vsAccountIns  :: ![AccountIn]
   , vsAccountOuts :: ![AccountOut]
   }

type VerifySumM = ReaderT VerifyState (Except ToilVerFailure)

runVerifySumM ::
       [(TxIn, TxOutAux)]
    -> [TxOut]
    -> [AccountIn]
    -> [AccountOut]
    -> VerifySumM a
    -> Either ToilVerFailure a
runVerifySumM resolvedInputs outputs acInputs acOutputs action =
    runExcept . runReaderT action $
        VerifyState resolvedInputs
                    outputs
                    acInputs
                    acOutputs

verifySum :: VerifySumM TxFee
verifySum = do
    txfee <- verifySumOfSeal
    verifyIssueState
    return txfee

verifySumOfSeal :: VerifySumM TxFee
verifySumOfSeal = do
    utxoInputs <- asks (map txOutSeal . filter isSealTxOut . fmap (toaOut . snd) . vsUtxoInputs)
    acInputs <- asks (map (cgSeal . riValue) . vsAccountIns)
    when (null utxoInputs && null acInputs) $ throwError ToilMissingSealCoins

    utxoOutputs <- asks (map txOutSeal . filter isSealTxOut . vsUtxoOutputs)
    acOutputs <- asks (map (cgSeal . doValue) . vsAccountOuts)
    let mTxFee = TxFee <$> rightToMaybe (integerToMoney (inpSum - outSum))
        outSum = sumMoneys (utxoOutputs <> acOutputs)
        inpSum = sumMoneys (utxoInputs <> acInputs)

    case mTxFee of
        Nothing -> throwError $ ToilOutGreaterThanIn inpSum outSum
        Just txFee -> return txFee

inconsistentTxAux :: Text -> VerifySumM a
inconsistentTxAux = throwError . ToilInconsistentTxAux

checkCurrenyEqual :: Currency c => [c] -> [c] -> Bool
checkCurrenyEqual coins1 coins2 = (sumMoneys coins1) == (sumMoneys coins2)

verifyIssueCert :: Maybe IssueCert -> VerifySumM ()
verifyIssueCert mcert = do
    utxoGoldInputs <- asks (map txOutGold . filter isGoldTxOut . fmap (toaOut . snd) . vsUtxoInputs)
    acGoldInputs <- asks (map (cgGold . riValue) . vsAccountIns)
    let inGolds = utxoGoldInputs <> acGoldInputs

    utxoGoldOutputs <- asks (map txOutGold . filter isGoldTxOut . vsUtxoOutputs)
    acGoldOutputs <- asks (map (cgGold . doValue) . vsAccountOuts)
    let outGolds = utxoGoldOutputs <> acGoldOutputs

    utxoDollarInputs <- asks (map txOutDollar . filter isDollarTxOut . fmap (toaOut . snd) . vsUtxoInputs)
    acDollarInputs <- asks (map (cgDollar . riValue) . vsAccountIns)
    let inDollars = utxoDollarInputs <> acDollarInputs

    utxoDollarOutputs <- asks (map txOutDollar . filter isDollarTxOut . vsUtxoOutputs)
    acDollarOutputs <- asks (map (cgDollar . doValue) . vsAccountOuts)
    let outDollars = utxoDollarOutputs <> acDollarOutputs

    let go (Just GoldCoinIssueCert{..}) = do
            unless (checkCurrenyEqual (issuedGolds : inGolds) outGolds) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCurrenyEqual inDollars outDollars) $
                inconsistentTxAux "GoldDollar not equal"

        go (Just GoldDollarIssueCert{..}) = do
            unless (checkCurrenyEqual inGolds (lockedGolds : outGolds)) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCurrenyEqual (issuedDollars : inDollars) outDollars) $
                inconsistentTxAux "GoldDollar not equal"

        go (Just GoldDollarDestroyCert{..}) = do
            unless (checkCurrenyEqual (unlockedGolds : inGolds) outGolds) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCurrenyEqual inDollars (destroyedDollars : outDollars)) $
                inconsistentTxAux "GoldDollar not equal"

        go Nothing = do
            unless (checkCurrenyEqual inGolds outGolds) $
                inconsistentTxAux "GoldCoin not equal"
            unless (checkCurrenyEqual inDollars outDollars) $
                inconsistentTxAux "GoldDollar not equal"

    go mcert

verifyIssueState :: VerifySumM ()
verifyIssueState = do
    inStates <- asks (map txOutState . filter isStateTxOut . fmap (toaOut . snd) . vsUtxoInputs)
    outStates <- asks (map (\txOut -> (txOutCert txOut, txOutState txOut)) . filter isStateTxOut . vsUtxoOutputs)

    go inStates outStates
    where
        stateError = inconsistentTxAux "invalid seal state output!"

        go [(GoldCoinState c1)] [(cert, (GoldCoinState c2))] = handle cert
            where
                handle GoldCoinIssueCert {..} = do
                    unless (checkCurrenyEqual [c1, issuedGolds] [c2]) $
                        inconsistentTxAux "cert GoldCoin not equal"
                    verifyIssueCert $ Just cert
                handle _ = inconsistentTxAux "invalid seal cert"

        go [(GoldDollarState total1 locked1)] [(cert, (GoldDollarState total2 locked2))] = handle cert
            where
                handle GoldDollarIssueCert{..} = do
                    unless (checkCurrenyEqual [total1, issuedDollars] [total2]) $
                        inconsistentTxAux "cert GoldDollar not equal"
                    unless (checkCurrenyEqual [locked1, lockedGolds] [locked2]) $
                        inconsistentTxAux "GoldDollarIssueCert lockedGolds invalid"
                    verifyIssueCert $ Just cert

                handle GoldDollarDestroyCert{..} = do
                    unless (checkCurrenyEqual [total2, destroyedDollars] [total1]) $
                        inconsistentTxAux "invalid GoldDollarDestroyCert"
                    unless (checkCurrenyEqual [locked1] [unlockedGolds, locked2]) $
                        inconsistentTxAux "GoldDollarIssueCert lockedGolds invalid"
                    verifyIssueCert $ Just cert

                handle _ = inconsistentTxAux "unmatched cert"

        go [] [] = verifyIssueCert Nothing

        go _ _ = stateError
