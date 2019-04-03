-- | Functions for sanity checking the GState DB.

module Seal.DB.Block.GState.SanityCheck
       ( sanityCheckDB
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Seal.Chain.Genesis (GenesisData)
import           Seal.DB.Class (MonadDBRead)
import           Seal.DB.GState.Stakes (getRealTotalStake)
import           Seal.DB.Txp (sanityCheckStakes, sanityCheckUtxo)
import           Seal.Util.AssertMode (inAssertMode)
import           Seal.Util.Wlog (WithLogger)

sanityCheckDB ::
       ( MonadMask m
       , WithLogger m
       , MonadDBRead m
       , MonadUnliftIO m
       )
    => GenesisData -> m ()
sanityCheckDB = inAssertMode . sanityCheckGStateDB

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall m.
       ( MonadDBRead m
       , MonadUnliftIO m
       , WithLogger m
       )
    => GenesisData
    -> m ()
sanityCheckGStateDB genesisData = do
    sanityCheckStakes
    sanityCheckUtxo genesisData =<< getRealTotalStake
