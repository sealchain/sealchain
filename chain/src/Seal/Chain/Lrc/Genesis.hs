-- | Computation of LRC genesis data.

module Seal.Chain.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Seal.Chain.Genesis as Genesis (Config (..),
                     configBootStakeholders, configEpochSlots, configFtsSeed)
import           Seal.Chain.Lrc.Fts (followTheSatoshi)
import           Seal.Chain.Txp (Utxo, genesisUtxo, utxoToStakes)
import           Seal.Core (SlotLeaders)


-- | Compute leaders of the 0-th epoch from initial shared seed and stake distribution.
genesisLeaders :: Genesis.Config -> SlotLeaders
genesisLeaders genesisConfig = followTheSatoshiUtxo
    genesisConfig
    (genesisUtxo $ configGenesisData genesisConfig)

-- This should not be exported unless it is *needed* elsewhere
-- NB: here we rely on the ordering produced by HM.toList; if it changes,
-- `genesisLeaders` might start producing different results. Be careful with
-- this code!
--
followTheSatoshiUtxo :: Genesis.Config -> Utxo -> SlotLeaders
followTheSatoshiUtxo genesisConfig utxo =
    followTheSatoshi
            (configEpochSlots genesisConfig)
            (configFtsSeed genesisConfig)
        $ HM.toList
        $ utxoToStakes (configBootStakeholders genesisConfig) utxo
