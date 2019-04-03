{-# LANGUAGE DataKinds #-}

-- | Various constraints needed for block processing.

module Seal.Network.Block.WorkMode
    ( BlockInstancesConstraint
    , BlockWorkMode
    ) where

import           Universum

import           Data.Default (Default)

import           Seal.Binary.Class (Bi)
import           Seal.Chain.Block (HasBlockConfiguration, HasSlogContext,
                     LastKnownHeader, LastKnownHeaderTag)
import           Seal.Chain.Security (SecurityParams)
import           Seal.Core.Context (HasPrimaryKey)
import           Seal.Core.JsonLog (CanJsonLog)
import           Seal.DB.Block (LrcModeFull)
import           Seal.DB.Txp (GenericTxpLocalData, MempoolExt, MonadTxpLocal,
                     TxpHolderTag)
import           Seal.DB.Update (UpdateContext)
import           Seal.Infra.Communication.Protocol (Message)
import           Seal.Infra.Recovery.Info (MonadRecoveryInfo)
import           Seal.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Seal.Infra.Shutdown.Class (HasShutdownContext)
import           Seal.Infra.StateLock (StateLock, StateLockMetrics)
import           Seal.Infra.Util.JsonLog.Events (MemPoolModifyReason)
import           Seal.Network.Block.RetrievalQueue (BlockRetrievalQueue,
                     BlockRetrievalQueueTag)
import           Seal.Network.Block.Types (MsgBlock, MsgGetBlocks, MsgGetHeaders,
                     MsgHeaders)
import           Seal.Util.Util (HasLens, HasLens')
import           Seal.Util.Wlog (WithLogger)

-- | These instances are implemented in @Seal.Binary.Communication@,
-- and @Seal.Communication.Limits@, which are unavailable at this
-- point, hence we defer providing them to the calling site.
type BlockInstancesConstraint =
    ( Each '[Bi]
        [ MsgGetHeaders
        , MsgHeaders
        , MsgGetBlocks
        , MsgBlock ]
    , Each '[Message]
        [ MsgGetHeaders
        , MsgHeaders
        , MsgGetBlocks
        , MsgBlock ]
    )

-- | A subset of @WorkMode@.
type BlockWorkMode ctx m =
    ( BlockInstancesConstraint

    , Default (MempoolExt m)

    , LrcModeFull ctx m
    , MonadRecoveryInfo ctx m
    , MonadTxpLocal m

    , HasPrimaryKey ctx
    , HasShutdownContext ctx
    , HasSlogContext ctx

    , HasLens BlockRetrievalQueueTag ctx BlockRetrievalQueue
    , HasLens LastKnownHeaderTag ctx LastKnownHeader
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    , HasLens TxpHolderTag ctx (GenericTxpLocalData (MempoolExt m))
    , HasLens' ctx SecurityParams
    , HasLens' ctx StateLock
    , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
    , HasLens' ctx UpdateContext

    , CanJsonLog m
    , WithLogger m

    , HasBlockConfiguration
    )
