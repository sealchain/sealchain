-- | Higher-level DB functionality.

module Seal.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Seal.Chain.Block (genesisBlock0, headerHash)
import           Seal.Chain.Genesis as Genesis (Config (..))
import           Seal.Chain.Lrc (genesisLeaders)
import           Seal.Chain.Update (BlockVersionData)
import           Seal.DB.Block (prepareBlockDB)
import           Seal.DB.Class (MonadDB, MonadDBRead (..))
import           Seal.DB.Lrc (prepareLrcDB)
import           Seal.DB.Update (getAdoptedBVData)
import           Seal.GState.GState (prepareGStateDB)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m
     . (MonadReader ctx m, MonadDB m)
    => Genesis.Config
    -> m ()
initNodeDBs genesisConfig = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB genesisConfig initialTip
    prepareLrcDB genesisConfig
  where
    gb = genesisBlock0 (configProtocolMagic genesisConfig)
                       (configGenesisHash genesisConfig)
                       (genesisLeaders genesisConfig)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
