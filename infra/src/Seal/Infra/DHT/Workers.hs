{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Seal.Infra.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Formatting (sformat, (%))
import           Network.Kademlia (takeSnapshot)
import           UnliftIO (MonadUnliftIO)

import           Seal.Binary.Class (serialize)
import           Seal.Core (BlockCount, kEpochSlots)
import           Seal.Core.Slotting (MonadSlots, flattenSlotId, slotIdF)
import           Seal.Infra.Binary.DHTModel ()
import           Seal.Infra.DHT.Constants (kademliaDumpInterval)
import           Seal.Infra.DHT.Real.Types (KademliaDHTInstance (..))
import           Seal.Infra.Diffusion.Types (Diffusion)
import           Seal.Infra.Recovery.Info (MonadRecoveryInfo, recoveryCommGuard)
import           Seal.Infra.Reporting (MonadReporting)
import           Seal.Infra.Shutdown (HasShutdownContext)
import           Seal.Infra.Slotting.Util (defaultOnNewSlotParams, onNewSlot)
import           Seal.Util.Wlog (WithLogger, logNotice)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots ctx m
    , MonadIO m
    , MonadUnliftIO m
    , MonadMask m
    , MonadRecoveryInfo ctx m
    , MonadReader ctx m
    , MonadReporting m
    , HasShutdownContext ctx
    )

dhtWorkers
    :: DhtWorkMode ctx m
    => BlockCount
    -> KademliaDHTInstance -> [Diffusion m -> m ()]
dhtWorkers k kademliaInst@KademliaDHTInstance {..} =
    [ dumpKademliaStateWorker k kademliaInst ]

dumpKademliaStateWorker
    :: DhtWorkMode ctx m
    => BlockCount
    -> KademliaDHTInstance
    -> Diffusion m
    -> m ()
dumpKademliaStateWorker k kademliaInst _ = onNewSlot epochSlots onsp $ \slotId ->
    when (isTimeToDump slotId) $ recoveryCommGuard k "dump kademlia state" $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        case dumpFile of
            Just fp -> liftIO . BSL.writeFile fp . serialize $ snapshot
            Nothing -> return ()
  where
    epochSlots = kEpochSlots k
    onsp = defaultOnNewSlotParams
    isTimeToDump slotId = flattenSlotId epochSlots slotId `mod` kademliaDumpInterval == 0
