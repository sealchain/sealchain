{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

-- | High level workers.

module Seal.Worker
       ( allWorkers
       ) where

import           Universum

import           Seal.Worker.Block (blkWorkers)
-- Message instances.
import           Seal.Chain.Genesis as Genesis (Config, configEpochSlots)
import           Seal.Chain.Txp (TxpConfiguration)
import           Seal.Context (NodeContext (..))
import           Seal.Infra.Diffusion.Types (Diffusion)
import           Seal.Infra.Network.CLI (launchStaticConfigMonitoring)
import           Seal.Infra.Network.Types (NetworkConfig (..))
import           Seal.Infra.Slotting (logNewSlotWorker)
import           Seal.Launcher.Resource (NodeResources (..))
import           Seal.Worker.Delegation (dlgWorkers)
import           Seal.Worker.Ssc (sscWorkers)
import           Seal.Worker.Update (usWorkers)
import           Seal.WorkMode (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m . WorkMode ctx m
    => Genesis.Config
    -> TxpConfiguration
    -> NodeResources ext
    -> [ (Text, Diffusion m -> m ()) ]
allWorkers genesisConfig txpConfig NodeResources {..} = mconcat
    [ sscWorkers genesisConfig
    , usWorkers genesisConfig
    , blkWorkers genesisConfig txpConfig
    , dlgWorkers
    , [ ("proper slotting", properSlottingWorker), ("static config", staticConfigMonitoringWorker) ]
    ]
  where
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorker =
        const $ logNewSlotWorker $ configEpochSlots genesisConfig
    staticConfigMonitoringWorker = const (launchStaticConfigMonitoring topology)
