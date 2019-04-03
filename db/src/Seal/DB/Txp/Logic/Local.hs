{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Logic for local processing of transactions.
-- Local transaction is a transaction which has not yet been added to the blockchain.

module Seal.DB.Txp.Logic.Local
       ( TxpProcessTransactionMode
       , txProcessTransaction
       , txProcessTransactionNoLock
       , txNormalize
       , txGetPayload

       -- * Utils for transaction processing and mempool normalization
       , txProcessTransactionAbstract
       , txNormalizeAbstract
       ) where

import           Universum

import           Control.Monad.Except (mapExceptT, runExceptT, throwError)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import           Formatting (build, sformat, (%))

import           Seal.Chain.Block (HeaderHash)
import           Seal.Chain.Contract (PactModifier)
import           Seal.Chain.Genesis as Genesis (Config (..), configEpochSlots)
import           Seal.Chain.Txp (ExtendedLocalToilM, LocalToilState (..),
                     LocalToilEnv (..), MemPool, ToilVerFailure (..), 
                     TxAux (..), TxId, TxUndo, TxpConfiguration (..), 
                     UndoMap, Utxo, AccountModifier, UtxoModifier, 
                     extendLocalToilM, mpLocalTxs, normalizeToil,
                     processTx, topsortTxs, utxoToLookup)
import           Seal.Chain.Update (BlockVersionData)
import           Seal.Core.JsonLog (CanJsonLog (..))
import           Seal.Core.JsonLog.LogEvents (MemPoolModifyReason (..))
import           Seal.Core.Reporting (reportError)
import           Seal.Core.Slotting (SlotId, SlotCount, MonadSlots (..))
import           Seal.Crypto (WithHash (..))
import           Seal.DB.Class (MonadGState (..))
import qualified Seal.DB.GState.Common as GS
import           Seal.DB.GState.Lock (Priority (..), StateLock, StateLockMetrics,
                     withStateLock)
import           Seal.DB.Txp.Account (getAccountStateRoot)
import           Seal.DB.Txp.Pact (getPactStateRoot)
import           Seal.DB.Txp.Logic.Common (buildUtxo)
import           Seal.DB.Txp.MemState (GenericTxpLocalData (..), MempoolExt,
                     MonadTxpMem, TxpLocalWorkMode, getLocalTxsMap, getAccountModifier,
                     getPactModifier, getLocalUndos, getMemPool, getTxpExtra, getTxpTip, 
                     getUtxoModifier, setTxpLocalData, withTxpLocalData)
import           Seal.Mpt.MerklePatricia (getMPDB)
import           Seal.Contract.Persist.MPTree (initMPtree)
import           Seal.Util.Util (HasLens')
import           Seal.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog,
                     logDebug, logError, logWarning)

type TxpProcessTransactionMode ctx m =
    ( TxpLocalWorkMode ctx m
    , HasLens' ctx StateLock
    , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
    , MempoolExt m ~ ()
    , CanJsonLog m
    )

-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: ( TxpProcessTransactionMode ctx m)
    => Genesis.Config -> TxpConfiguration -> (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransaction genesisConfig txpConfig itw =
    withStateLock LowPriority ProcessTransaction $ \__tip -> txProcessTransactionNoLock genesisConfig txpConfig itw

-- | Unsafe version of 'txProcessTransaction' which doesn't take a
-- lock. Can be used in tests.
txProcessTransactionNoLock
    :: forall ctx m.
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       )
    => Genesis.Config
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionNoLock genesisConfig txpConfig = txProcessTransactionAbstract
    (configEpochSlots genesisConfig)
    buildContext
    processTxHoisted
  where
    buildContext :: Utxo -> TxAux -> m ()
    buildContext _ _ = pure ()

    processTxHoisted ::
           BlockVersionData
        -> SlotId
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure (ExtendedLocalToilM () () m) TxUndo
    processTxHoisted bvd =
        mapExceptT extendLocalToilM
            ... (processTx (configProtocolMagic genesisConfig) txpConfig bvd)

txProcessTransactionAbstract ::
       forall extraEnv extraState ctx m a.
       (TxpLocalWorkMode ctx m, MempoolExt m ~ extraState)
    => SlotCount
    -> (Utxo -> TxAux -> m extraEnv)
    -> (BlockVersionData -> SlotId -> (TxId, TxAux) -> ExceptT ToilVerFailure (ExtendedLocalToilM extraEnv extraState m) a)
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionAbstract epochSlots buildEnv txAction itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    -- Note: we need to read tip from the DB and check that it's the
    -- same as the one in mempool. That's because mempool state is
    -- valid only with respect to the tip stored there. Normally tips
    -- will match, because whenever we apply/rollback blocks we
    -- normalize mempool. However, there is a corner case when we
    -- receive an unexpected exception after modifying GState and
    -- before normalization. In this case normalization can fail and
    -- tips will differ. Rejecting transactions in this case should be
    -- fine, because the fact that we receive exceptions likely
    -- indicates that something is bad and we have more serious issues.
    --
    -- Also note that we don't need to use a snapshot here and can be
    -- sure that GState won't change, because changing it requires
    -- 'StateLock' which we own inside this function.
    tipDB <- lift GS.getTip
    slot <- note ToilSlotUnknown =<< getCurrentSlot epochSlots
    utxoModifier <- withTxpLocalData getUtxoModifier
    utxo <- buildUtxo utxoModifier [txAux]
    extraEnv <- lift $ buildEnv utxo txAux
    bvd <- gsAdoptedBVData

    am <- withTxpLocalData getAccountModifier
    pm <- withTxpLocalData getPactModifier
    mp <- withTxpLocalData getMemPool
    undo <- withTxpLocalData getLocalUndos
    tip <- withTxpLocalData getTxpTip
    extra <- withTxpLocalData getTxpExtra
    pRes <- lift $ 
              launchNamedPureLog id $ 
              processTransactionPure bvd slot utxo extraEnv tipDB itw (utxoModifier, mp, undo, am, pm, tip, extra)
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: " %build) txId
            throwError er
        Right txpData -> do
            logDebug
                (sformat ("Transaction is processed successfully: " %build) txId)
            lift $ withTxpLocalData $ flip setTxpLocalData txpData
  where
    processTransactionPure
        :: BlockVersionData
        -> SlotId
        -> Utxo
        -> extraEnv
        -> HeaderHash
        -> (TxId, TxAux)
        -> (UtxoModifier, MemPool, UndoMap, AccountModifier, PactModifier, HeaderHash, extraState)
        -> NamedPureLogger m (Either ToilVerFailure (UtxoModifier, MemPool, UndoMap, AccountModifier, PactModifier, HeaderHash, extraState))
    processTransactionPure bvd curSlot utxo eEnv tipDB tx (um, mp, undo, am, pm, tip, extraState)
        | tipDB /= tip = pure . Left $ ToilTipsMismatch tipDB tip
        | otherwise = do
            acctMPDB <- lift $ getMPDB =<< getAccountStateRoot tip
            pactMPDB <- lift $ getMPDB =<< getPactStateRoot tip
            let initialEnv = LocalToilEnv { _lteUtxo = (utxoToLookup utxo)
                                          , _lteAcctMPDB = acctMPDB
                                          , _ltePactMPDB = pactMPDB
                                          }
            let initialState = LocalToilState { _ltsMemPool = mp
                                              , _ltsUtxoModifier = um
                                              , _ltsUndos = undo
                                              , _ltsAcctModifier = am 
                                              , _ltsPactModifier = pm 
                                              }
            res :: (Either ToilVerFailure a, (LocalToilState, extraState)) <-
                    usingStateT (initialState, extraState) $
                    usingReaderT (initialEnv, eEnv) $
                    runExceptT $
                    txAction bvd curSlot tx
            case res of
                (Left er, _) -> pure $ Left er
                (Right _, (LocalToilState {..}, newExtraState)) -> pure $ Right
                    (_ltsUtxoModifier, _ltsMemPool, _ltsUndos, _ltsAcctModifier, _ltsPactModifier, tip, newExtraState)
    -- REPORT:ERROR Tips mismatch in txp.
    reportTipMismatch action = do
        res <- action
        res <$ case res of
            (Left err@(ToilTipsMismatch {})) -> reportError (pretty err)
            _                                -> pass

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: forall ctx m.
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       )
    => Genesis.Config -> TxpConfiguration -> m ()
txNormalize genesisConfig txpConfig =
    txNormalizeAbstract (configEpochSlots genesisConfig) buildContext
        $ normalizeToilHoisted
  where
    buildContext :: Utxo -> [TxAux] -> m ()
    buildContext _ _ = pure ()

    normalizeToilHoisted
        :: BlockVersionData
        -> SlotId
        -> HashMap TxId TxAux
        -> ExtendedLocalToilM () () m ()
    normalizeToilHoisted bvd slot txs =
        extendLocalToilM
            $ normalizeToil (configProtocolMagic genesisConfig) txpConfig bvd slot
            $ HM.toList txs

txNormalizeAbstract ::
       (TxpLocalWorkMode ctx m, MempoolExt m ~ extraState)
    => SlotCount
    -> (Utxo -> [TxAux] -> m extraEnv)
    -> (BlockVersionData -> SlotId -> HashMap TxId TxAux -> ExtendedLocalToilM extraEnv extraState m ())
    -> m ()
txNormalizeAbstract epochSlots buildEnv normalizeAction =
    getCurrentSlot epochSlots >>= \case
        Nothing -> do
            tip <- GS.getTip

            pactStateRoot <- getPactStateRoot tip
            pactMPDB <- getMPDB pactStateRoot

            -- Clear and update tip
            withTxpLocalData $ flip setTxpLocalData (mempty, def, mempty, mempty, (initMPtree pactMPDB), tip, def)
        Just slot -> do
            globalTip <- GS.getTip
            localTxs <- withTxpLocalData getLocalTxsMap
            let txAuxes = toList localTxs
            utxo <- buildUtxo mempty txAuxes
            extraEnv <- buildEnv utxo txAuxes
            bvd <- gsAdoptedBVData

            acctMPDB <- getMPDB =<< getAccountStateRoot globalTip
            pactMPDB <- getMPDB =<< getPactStateRoot globalTip

            let initialEnv = LocalToilEnv { _lteUtxo = (utxoToLookup utxo)
                                          , _lteAcctMPDB = acctMPDB
                                          , _ltePactMPDB = pactMPDB
                                          }
            let initialState =
                    LocalToilState
                        { _ltsMemPool = def
                        , _ltsUtxoModifier = mempty
                        , _ltsUndos = mempty
                        , _ltsAcctModifier = mempty
                        , _ltsPactModifier = initMPtree pactMPDB
                        }
            (LocalToilState {..}, newExtraState) <-
                launchNamedPureLog id $
                execStateT
                    (runReaderT
                         (normalizeAction bvd slot localTxs)
                         (initialEnv, extraEnv))
                    (initialState, def)
            withTxpLocalData $ flip setTxpLocalData
                ( _ltsUtxoModifier
                , _ltsMemPool
                , _ltsUndos
                , _ltsAcctModifier
                , _ltsPactModifier
                , globalTip
                , newExtraState)

-- | Get 'TxPayload' from mempool to include into a new block which
-- will be based on the given tip. In something goes wrong, empty
-- payload is returned. That's because we would sooner create an empty
-- block to maintain decent chain quality than skip block creation.
--
-- We need to explicitly check that tip matches, even though we do
-- mempool normalization whenever we apply/rollback a block. That's
-- because we can't make them both atomically, i. e. can't guarantee
-- that either none or both of them will be done.
txGetPayload :: (MonadIO m, MonadTxpMem ext ctx m, WithLogger m) => HeaderHash -> m [TxAux]
txGetPayload neededTip = do
    (view mpLocalTxs -> memPool, memPoolTip) <- withTxpLocalData $ \(TxpLocalData{..}) ->
        (,) <$> readTVar txpMemPool <*> readTVar txpTip
    let tipMismatchMsg =
            sformat
                ("txGetPayload: tip mismatch (in DB: )"%build%
                 ", (in mempool: "%build%")")
                neededTip memPoolTip
    let topsortFailMsg = "txGetPayload: topsort failed!"
    let convertTx (txId, txAux) = WithHash (taTx txAux) txId
    case (memPoolTip == neededTip, topsortTxs convertTx $ HM.toList memPool) of
        (False, _)       -> [] <$ logWarning tipMismatchMsg
        (True, Nothing)  -> [] <$ logError topsortFailMsg
        (True, Just res) -> return $ map snd res
