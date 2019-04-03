{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

-- | Logic for global processing of transactions.  Global transaction
-- is a transaction which has already been added to the blockchain.

module Seal.DB.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ProcessBlundsSettings (..)
       , processBlunds
       , applyBlocksWith
       , blundToAuxNUndo
       ) where

import           Universum

import           Control.Lens (magnify, zoom)
import           Control.Monad.Except (throwError)
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))

import           Seal.Contract.Persist.MPTree (initMPtree)
import           Seal.Chain.Block (ComponentBlock (..), headerHash)
import           Seal.Chain.Contract (PactModifier, commitPactModify)
import           Seal.Chain.Genesis as Genesis (Config (..),
                     configBootStakeholders, configEpochSlots)
import           Seal.Chain.Genesis (GenesisWStakeholders)
import           Seal.Chain.Txp (ExtendedGlobalToilM, GlobalToilEnv (..),
                     GlobalToilM, GlobalToilState (..), StakesView (..),
                     ToilVerFailure, TxAux (..), TxUndo, TxpConfiguration (..),
                     TxpUndo, Utxo, UtxoModifier, AccountModifier, 
                     applyToil, defGlobalToilState, flattenTxPayload, 
                     gtsUtxoModifier, gtsAcctModifier, gtsPactModifier,
                     gteAcctMPDB, rollbackToil, runGlobalToilM, 
                     utxoToLookup, verifyToil)
import           Seal.Core.Slotting (SlotCount, flattenEpochOrSlot, unflattenSlotId)
import           Seal.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Seal.Core.Exception (assertionFailed)
import           Seal.Crypto (ProtocolMagic)
import           Seal.DB (SomeBatchOp (..))
import           Seal.DB.Class (gsAdoptedBVData)
import           Seal.DB.GState.Common (getTip)
import           Seal.DB.GState.Stakes (getRealStake, getRealTotalStake)
import           Seal.DB.Txp.Account (AccountStateRootOp (..), getAccountStateRoot)
import           Seal.DB.Txp.Pact (PactStateRootOp (..), getPactStateRoot)
import           Seal.DB.Txp.Logic.Common (buildUtxo, buildUtxoForRollback)
import           Seal.DB.Txp.Settings (TxpBlock, TxpBlund, TxpCommonMode,
                     TxpGlobalApplyMode, TxpGlobalRollbackMode,
                     TxpGlobalSettings (..), TxpGlobalVerifyMode)
import           Seal.DB.Txp.Stakes (StakesOp (..))
import           Seal.DB.Txp.Utxo (UtxoOp (..))
import           Seal.Mpt.MerklePatricia (MPDB (..), getMPDB, commitAccountModifier)
import           Seal.Util.AssertMode (inAssertMode)
import qualified Seal.Util.Modifier as MM
import           Seal.Util.Wlog (launchNamedPureLog)

----------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------

-- | Settings used for global transactions data processing used by a
-- simple full node.
txpGlobalSettings :: Genesis.Config -> TxpConfiguration -> TxpGlobalSettings
txpGlobalSettings genesisConfig txpConfig = TxpGlobalSettings
    { tgsVerifyBlocks   = verifyBlocks pm slotCount txpConfig
    , tgsApplyBlocks    = applyBlocksWith
        pm
        slotCount
        txpConfig
        (processBlundsSettings False $ applyToil bootStakeholders)
    , tgsRollbackBlocks = rollbackBlocks bootStakeholders
    }
  where
    pm               = configProtocolMagic genesisConfig
    slotCount        = configEpochSlots genesisConfig
    bootStakeholders = configBootStakeholders genesisConfig

----------------------------------------------------------------------------
-- Verify
----------------------------------------------------------------------------

verifyBlocks ::
       forall ctx m. (TxpGlobalVerifyMode ctx m)
    => ProtocolMagic
    -> SlotCount
    -> TxpConfiguration
    -> Bool
    -> OldestFirst NE TxpBlock
    -> m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
verifyBlocks pm slotCount _ verifyAllIsKnown newChain = runExceptT $ do
    bvd <- gsAdoptedBVData
    totalStake <- getRealTotalStake

    tip <- getTip
    acctMPDB <- getMPDB =<< getAccountStateRoot tip
    pactMPDB <- getMPDB =<< getPactStateRoot tip

    let verifyToil' :: [TxAux] -> GlobalToilM m (Either ToilVerFailure TxpUndo)
        verifyToil' = runExceptT
            . verifyToil pm bvd slot verifyAllIsKnown

        baseGte = 
            GlobalToilEnv
                { _gteUtxo = utxoToLookup M.empty
                , _gteTotalStake = totalStake
                , _gteStakeGetter = getRealStake
                , _gteAcctMPDB = acctMPDB
                , _gtePactMPDB = pactMPDB
                }

        foldStep ::
               (UtxoModifier, [TxpUndo], AccountModifier, PactModifier)
            -> TxpBlock
            -> ExceptT ToilVerFailure m (UtxoModifier, [TxpUndo], AccountModifier, PactModifier)
        foldStep (utxoModifier, undos, acctModifier, pactModifier) (convertPayload -> txAuxes) = do
            baseUtxo <- utxoToLookup <$> buildUtxo utxoModifier txAuxes
            let gte = baseGte {_gteUtxo = baseUtxo}
            let gts = GlobalToilState
                          { _gtsUtxoModifier = utxoModifier
                          , _gtsStakesView = def
                          , _gtsAcctModifier = acctModifier
                          , _gtsPactModifier = pactModifier
                          }
            res <- lift $ runGlobalToilM gte gts (verifyToil' txAuxes)
            case res of
                (Left err, _)         -> throwError err
                (Right txpUndo, gts') ->
                    return 
                      ( gts' ^. gtsUtxoModifier
                      , (txpUndo : undos) 
                      , gts' ^. gtsAcctModifier 
                      , gts' ^. gtsPactModifier)
        -- 'NE.fromList' is safe here, because there will be at least
        -- one 'foldStep' (since 'newChain' is not empty) and it will
        -- either fail (and then 'convertRes' will not be called) or
        -- will prepend something to the result.
        convertRes :: (UtxoModifier, [TxpUndo], AccountModifier, PactModifier) -> OldestFirst NE TxpUndo
        convertRes (_, undos, _, _) = OldestFirst . NE.fromList . reverse $ undos
    convertRes <$> foldM foldStep (mempty, mempty, mempty, initMPtree pactMPDB) newChain
  where
    getSlotId = (unflattenSlotId slotCount) . (flattenEpochOrSlot slotCount)
    slot = getSlotId $ NE.last (getOldestFirst newChain)
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
    convertPayload (ComponentBlockGenesis _)      = []

----------------------------------------------------------------------------
-- General processing
----------------------------------------------------------------------------

data ProcessBlundsSettings extraEnv extraState m = ProcessBlundsSettings
    { pbsProcessSingle   :: TxpBlund -> m (ExtendedGlobalToilM extraEnv extraState m ())
    , pbsCreateEnv       :: Utxo -> [TxAux] -> m extraEnv
    , pbsExtraOperations :: extraState -> SomeBatchOp
    , pbsIsRollback      :: !Bool
    -- ^ This flag specifies whether we want to rollback transactions
    -- or apply them. It affects the way we construct base 'Utxo'. If
    -- we want to apply transactions, we should use 'buildUtxo' to
    -- resolved all their inputs. But if we want to rollback them, we
    -- should turn known outputs of transactions into 'Utxo'.
    }

processBlunds ::
       forall extraEnv extraState ctx m. (TxpCommonMode ctx m, Default extraState)
    => ProcessBlundsSettings extraEnv extraState m
    -> NE TxpBlund
    -> m SomeBatchOp
processBlunds ProcessBlundsSettings {..} blunds = do
    let toBatchOp ((gts, extra), mpdbBatchOp) =
            globalToilStateToBatch gts <> pbsExtraOperations extra <> mpdbBatchOp
    totalStake <- getRealTotalStake -- doesn't change

    tip <- getTip
    initialAcctMPDB <- getMPDB =<< getAccountStateRoot tip
    initialPactMPDB <- getMPDB =<< getPactStateRoot tip

    let initialGte = GlobalToilEnv
                         { _gteUtxo = utxoToLookup M.empty
                         , _gteTotalStake = totalStake
                         , _gteStakeGetter = getRealStake
                         , _gteAcctMPDB = initialAcctMPDB
                         , _gtePactMPDB = initialPactMPDB
                         }

    -- Note: base utxo also doesn't change, but we build it on each
    -- step (for different sets of transactions), because
    -- 'UtxoModifier' may accumulate some data and it may be more
    -- efficient.

    -- Another note: if we rollback transactions, we don't really need
    -- base utxo, but we have a sanity check in 'utxoDel' which forces
    -- us to construct base utxo here.
    let buildBaseUtxo :: UtxoModifier -> [TxAux] -> m Utxo
        buildBaseUtxo
            | pbsIsRollback = buildUtxoForRollback
            | otherwise = buildUtxo
        
        toMPDBBatchOp 
            :: GlobalToilState 
            -> GlobalToilEnv m 
            -> TxpBlund
            -> m (GlobalToilState, GlobalToilEnv m, SomeBatchOp)
        toMPDBBatchOp gts gte (blk, _) 
            | pbsIsRollback = return (gts, gte, mempty)
            | otherwise = do
                acctMPDB <- commitAccountModifier
                            (gts ^. gtsAcctModifier) 
                            (gte ^. gteAcctMPDB)
                pactMPDB <- liftIO $ commitPactModify (gts ^. gtsPactModifier)
                let hh = headerHash blk
                let batchOp = (SomeBatchOp $ 
                                PutAccountStateRoot hh $ stateRoot acctMPDB
                            ) <> 
                            (SomeBatchOp $ 
                                PutPactStateRoot hh $ stateRoot pactMPDB
                            )
                let gts' = gts { _gtsAcctModifier = mempty, _gtsPactModifier = initMPtree pactMPDB }
                let gte' = gte { _gteAcctMPDB = acctMPDB, _gtePactMPDB = pactMPDB }
                return (gts', gte', batchOp)

    let step ::
               ((GlobalToilState, extraState), GlobalToilEnv m, SomeBatchOp)
            -> TxpBlund
            -> m ((GlobalToilState, extraState), GlobalToilEnv m, SomeBatchOp)
        step ((gts, extraState), gte, batchOp) txpBlund = do
            processSingle <- pbsProcessSingle txpBlund
            let txAuxesAndUndos = blundToAuxNUndo txpBlund
                txAuxes = fst <$> txAuxesAndUndos
            baseUtxo <- buildBaseUtxo (gts ^. gtsUtxoModifier) txAuxes
            extraEnv <- pbsCreateEnv baseUtxo txAuxes

            let gte' = gte { _gteUtxo = utxoToLookup baseUtxo }
            (gts', extraState') <- launchNamedPureLog id $ flip execStateT (gts, extraState) .
                  usingReaderT (gte', extraEnv) $
                  processSingle

            (gts'', gte'', newBatchOp) <- toMPDBBatchOp gts' gte' txpBlund
            return ((gts'', extraState'), gte'', batchOp <> newBatchOp)
 
    let cc = ((defGlobalToilState initialPactMPDB, def), initialGte, mempty)
    (st, _, batchOp) <- foldM step cc blunds
    return $ toBatchOp (st, batchOp)

----------------------------------------------------------------------------
-- Apply and rollback
----------------------------------------------------------------------------

applyBlocksWith ::
       forall extraEnv extraState ctx m.
       (TxpGlobalApplyMode ctx m, Default extraState)
    => ProtocolMagic
    -> SlotCount
    -> TxpConfiguration
    -> ProcessBlundsSettings extraEnv extraState m
    -> OldestFirst NE TxpBlund
    -> m SomeBatchOp
applyBlocksWith pm slotCount txpConfig settings blunds = do
    let blocks = map fst blunds
    inAssertMode $ do
        verdict <- verifyBlocks pm slotCount txpConfig False blocks
        whenLeft verdict $
            assertionFailed .
            sformat ("we are trying to apply txp blocks which we fail to verify: "%build)
    processBlunds settings (getOldestFirst blunds)

processBlundsSettings ::
       forall m. Monad m
    => Bool
    -> ([(TxAux, TxUndo)] -> GlobalToilM m ())
    -> ProcessBlundsSettings () () m
processBlundsSettings isRollback pureAction =
    ProcessBlundsSettings
        { pbsProcessSingle = \txpBlund -> pure (processSingle txpBlund)
        , pbsCreateEnv = \_ _ -> pure ()
        , pbsExtraOperations = const mempty
        , pbsIsRollback = isRollback
        }
  where
    processSingle :: TxpBlund -> ExtendedGlobalToilM () () m ()
    processSingle = zoom _1 . magnify _1 . pureAction . blundToAuxNUndo

rollbackBlocks ::
       forall ctx m. (TxpGlobalRollbackMode ctx m)
    => GenesisWStakeholders
    -> NewestFirst NE TxpBlund
    -> m SomeBatchOp
rollbackBlocks bootStakeholders (NewestFirst blunds) = processBlunds
    (processBlundsSettings True (rollbackToil bootStakeholders))
    blunds

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert 'GlobalToilState' to batch of database operations.
globalToilStateToBatch :: GlobalToilState -> SomeBatchOp
globalToilStateToBatch GlobalToilState {..} =
    SomeBatchOp [SomeBatchOp utxoOps, SomeBatchOp stakesOps]
  where
    StakesView (HM.toList -> stakes) total = _gtsStakesView
    utxoOps =
        map DelTxIn (MM.deletions _gtsUtxoModifier) ++
        map (uncurry AddTxOut) (MM.insertions _gtsUtxoModifier)
    stakesOps = addTotalStakeOp $ map (uncurry PutFtsStake) stakes
    addTotalStakeOp =
        case total of
            Nothing -> identity
            Just x  -> (PutTotalStake x :)

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (ComponentBlockGenesis _ , _)        = []
blundToAuxNUndo (ComponentBlockMain _ payload, undo) = zip (flattenTxPayload payload) undo
