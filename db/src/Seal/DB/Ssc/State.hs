{-# LANGUAGE TypeFamilies #-}

module Seal.DB.Ssc.State
       ( mkSscState
       , module Seal.DB.Ssc.State.Global
       , module Seal.DB.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM

import           Seal.Chain.Ssc (SscState (..))
import           Seal.Core.Slotting (MonadSlots, SlotCount)
import           Seal.DB (MonadDBRead)
import           Seal.Util.Wlog (WithLogger)

-- Reexports
import           Seal.DB.Ssc.State.Global
import           Seal.DB.Ssc.State.Local

mkSscState
    :: forall ctx m
     . (WithLogger m, MonadDBRead m, MonadSlots ctx m)
    => SlotCount
    -> m SscState
mkSscState epochSlots = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData epochSlots
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
