{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

module Seal.WorkMode
       ( WorkMode
       , MinWorkMode

       -- * Actual modes
       , RealMode
       , RealModeContext(..)
       , EmptyMempoolExt
       ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl

import           Seal.Chain.Block (HasSlogContext (..), HasSlogGState (..))
import           Seal.Chain.Delegation (DelegationVar)
import           Seal.Chain.Ssc (SscMemTag, SscState)
import           Seal.Context (HasNodeContext (..), HasPrimaryKey (..),
                     HasSscContext (..), NodeContext)
import           Seal.Core.JsonLog (CanJsonLog (..))
import           Seal.Core.Reporting (HasMisbehaviorMetrics (..))
import           Seal.Core.Slotting (HasSlottingVar (..), MonadSlotsData)
import           Seal.DB (MonadGState (..), NodeDBs)
import           Seal.DB.Block (MonadBListener (..), dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault, onApplyBlocksStub,
                     onRollbackBlocksStub)
import           Seal.DB.Class (MonadDB (..), MonadDBRead (..))
import           Seal.DB.DB (gsAdoptedBVDataDefault)
import           Seal.DB.Rocks (dbDeleteDefault, dbGetDefault,
                     dbIterSourceDefault, dbPutDefault, dbWriteBatchDefault)
import           Seal.DB.Txp (GenericTxpLocalData, MempoolExt,
                     MonadTxpLocal (..), TxpHolderTag, txNormalize,
                     txProcessTransaction)
import           Seal.Infra.DHT.Real.Param (KademliaParams)
import           Seal.Infra.Network.Types (HasNodeType (..), getNodeTypeDefault)
import           Seal.Infra.Reporting (MonadReporting (..), Reporter (..))
import           Seal.Infra.Shutdown (HasShutdownContext (..))
import           Seal.Infra.Slotting.Class (MonadSlots (..))
import           Seal.Infra.Slotting.Impl (currentTimeSlottingSimple,
                     getCurrentSlotBlockingSimple,
                     getCurrentSlotInaccurateSimple, getCurrentSlotSimple)
import           Seal.Infra.Util.JsonLog.Events (HasJsonLogConfig (..),
                     JsonLogConfig, jsonLogDefault)
import           Seal.Util.Lens (postfixLFields)
import           Seal.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                     modifyLoggerNameDefault)
import           Seal.Util.UserPublic (HasUserPublic (..))
import           Seal.Util.UserSecret (HasUserSecret (..))
import           Seal.Util.Util (HasLens (..))
import           Seal.Util.Wlog (HasLoggerName (..), LoggerName)
import           Seal.WorkMode.Class (MinWorkMode, WorkMode)

data RealModeContext ext = RealModeContext
    { rmcNodeDBs       :: !NodeDBs
    , rmcSscState      :: !SscState
    , rmcTxpLocalData  :: !(GenericTxpLocalData ext)
    , rmcDelegationVar :: !DelegationVar
    , rmcJsonLogConfig :: !JsonLogConfig
    , rmcLoggerName    :: !LoggerName
    , rmcNodeContext   :: !NodeContext
    , rmcReporter      :: !(Reporter IO)
      -- ^ How to do reporting. It's in here so that we can have
      -- 'MonadReporting (RealMode ext)' in the mean-time, until we
      -- re-architecht the reporting system so that it's not built-in to the
      -- application's monad.
    }

type EmptyMempoolExt = ()

type RealMode ext = Mtl.ReaderT (RealModeContext ext) IO

makeLensesWith postfixLFields ''RealModeContext

instance HasLens NodeDBs (RealModeContext ext) NodeDBs where
    lensOf = rmcNodeDBs_L

instance HasLens NodeContext (RealModeContext ext) NodeContext where
    lensOf = rmcNodeContext_L

instance HasLens SscMemTag (RealModeContext ext) SscState where
    lensOf = rmcSscState_L

instance HasLens TxpHolderTag (RealModeContext ext) (GenericTxpLocalData ext) where
    lensOf = rmcTxpLocalData_L

instance HasLens DelegationVar (RealModeContext ext) DelegationVar where
    lensOf = rmcDelegationVar_L

instance HasNodeType (RealModeContext ext) where
    getNodeType = getNodeTypeDefault @KademliaParams

instance {-# OVERLAPPABLE #-}
    HasLens tag NodeContext r =>
    HasLens tag (RealModeContext ext) r
  where
    lensOf = rmcNodeContext_L . lensOf @tag

instance HasSscContext (RealModeContext ext) where
    sscContext = rmcNodeContext_L . sscContext

instance HasPrimaryKey (RealModeContext ext) where
    primaryKey = rmcNodeContext_L . primaryKey

instance HasMisbehaviorMetrics (RealModeContext ext) where
    misbehaviorMetrics = rmcNodeContext_L . misbehaviorMetrics

instance HasUserPublic (RealModeContext ext) where
    userPublic = rmcNodeContext_L . userPublic

instance HasUserSecret (RealModeContext ext) where
    userSecret = rmcNodeContext_L . userSecret

instance HasShutdownContext (RealModeContext ext) where
    shutdownContext = rmcNodeContext_L . shutdownContext

instance HasSlottingVar (RealModeContext ext) where
    slottingTimestamp = rmcNodeContext_L . slottingTimestamp
    slottingVar = rmcNodeContext_L . slottingVar

instance HasSlogContext (RealModeContext ext) where
    slogContext = rmcNodeContext_L . slogContext

instance HasSlogGState (RealModeContext ext) where
    slogGState = slogContext . scGState

instance HasNodeContext (RealModeContext ext) where
    nodeContext = rmcNodeContext_L

instance HasLoggerName' (RealModeContext ext) where
    loggerName = rmcLoggerName_L

instance HasJsonLogConfig (RealModeContext ext) where
    jsonLogConfig = rmcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName (RealMode ext) where
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog (RealMode ext) where
    jsonLog = jsonLogDefault

instance MonadSlotsData ctx (RealMode ext) => MonadSlots ctx (RealMode ext) where
    getCurrentSlot = getCurrentSlotSimple
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting = currentTimeSlottingSimple

instance MonadGState (RealMode ext) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadDBRead (RealMode ext) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

instance MonadDB (RealMode ext) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

instance MonadBListener (RealMode ext) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks nm _ blunds = onRollbackBlocksStub nm blunds

type instance MempoolExt (RealMode ext) = ext

instance MonadTxpLocal (RealMode ()) where
    txpNormalize = txNormalize
    txpProcessTx = txProcessTransaction

instance MonadReporting (RealMode ext) where
    report rt = Mtl.ask >>= liftIO . flip runReporter rt . rmcReporter
