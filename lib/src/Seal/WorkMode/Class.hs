{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

{-| The 'WorkMode' constraint, which is widely used throughout the codebase.
    It is a simple alias for a bunch of other useful constraints.
-}

module Seal.WorkMode.Class
       ( WorkMode
       , MinWorkMode
       ) where

import           Universum

import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Random as Rand
import           UnliftIO (MonadUnliftIO)

import           Seal.Chain.Block (HasBlockConfiguration, HasSlogContext,
                     HasSlogGState, MonadLastKnownHeader)
import           Seal.Chain.Delegation (HasDlgConfiguration, MonadDelegation)
import           Seal.Chain.Security (SecurityParams)
import           Seal.Chain.Ssc (HasSscConfiguration, MonadSscMem)
import           Seal.Chain.Update (HasUpdateConfiguration, UpdateParams)
import           Seal.Configuration (HasNodeConfiguration)
import           Seal.Context (BlockRetrievalQueue, BlockRetrievalQueueTag,
                     HasSscContext, StartTime, TxpGlobalSettings)
import           Seal.Core (HasPrimaryKey)
import           Seal.Core.JsonLog (CanJsonLog)
import           Seal.Core.Reporting (HasMisbehaviorMetrics, MonadReporting)
import           Seal.DB.Block (MonadBListener)
import           Seal.DB.Class (MonadDB, MonadGState)
import           Seal.DB.Lrc (HasLrcContext)
import           Seal.DB.Rocks (MonadRealDB)
import           Seal.DB.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem)
import           Seal.DB.Update (UpdateContext)
import           Seal.Infra.DHT.Real.Param (KademliaParams)
import           Seal.Infra.Network.Types (HasNodeType, NetworkConfig)
import           Seal.Infra.Recovery.Info (MonadRecoveryInfo)
import           Seal.Infra.Shutdown (HasShutdownContext)
import           Seal.Infra.Slotting.Class (MonadSlots)
import           Seal.Infra.StateLock (StateLock, StateLockMetrics)
import           Seal.Infra.Util.JsonLog.Events (MemPoolModifyReason)
import           Seal.Recovery.Types (MonadRecoveryHeader)
import           Seal.Util (HasLens, HasLens')
import           Seal.Util.Wlog (WithLogger)

-- | Bunch of constraints to perform work for real world distributed system.
type WorkMode ctx m
    = ( MinWorkMode m
      , MonadBaseControl IO m
      , Rand.MonadRandom m
      , MonadMask m
      , MonadSlots ctx m
      , MonadDB m
      , MonadRealDB ctx m
      , MonadGState m
      , MonadTxpLocal m
      , MonadTxpMem (MempoolExt m) ctx m
      , MonadDelegation ctx m
      , MonadSscMem ctx m
      , MonadRecoveryInfo ctx m
      , MonadRecoveryHeader ctx m
      , MonadLastKnownHeader ctx m
      , MonadBListener m
      , MonadReporting m
      , MonadReader ctx m
      , HasLens' ctx StartTime
      , HasLens' ctx StateLock
      , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
      , HasLens' ctx UpdateContext
      , HasLens' ctx UpdateParams
      , HasLens' ctx SecurityParams
      , HasLens' ctx TxpGlobalSettings
      , HasLens' ctx (NetworkConfig KademliaParams)
      , HasLens BlockRetrievalQueueTag ctx BlockRetrievalQueue
      , HasLrcContext ctx
      , HasSscContext ctx
      , HasMisbehaviorMetrics ctx
      , HasPrimaryKey ctx
      , HasShutdownContext ctx
      , HasSlogContext ctx
      , HasSlogGState ctx
      , HasNodeType ctx
      , HasSscConfiguration
      , HasDlgConfiguration
      )

-- | More relaxed version of 'WorkMode'.
type MinWorkMode m
    = ( WithLogger m
      , CanJsonLog m
      , MonadIO m
      , MonadUnliftIO m
      , HasUpdateConfiguration
      , HasNodeConfiguration
      , HasBlockConfiguration
      )
