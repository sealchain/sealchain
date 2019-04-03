{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Definitions for class of monads that capture logic of processing
-- delegate certificates (proxy secret keys).

module Seal.DB.Delegation.Var
       ( mkDelegationVar
       ) where

import           Universum

import qualified Data.Cache.LRU as LRU

import           Seal.Chain.Block (headerHash)
import           Seal.Chain.Delegation (DelegationVar, DelegationWrap (..),
                     HasDlgConfiguration, dlgCacheParam)
import           Seal.DB (MonadBlockDBRead)
import           Seal.DB.BlockIndex (getTipHeader)

-- | Make a new 'DelegationVar' and initialize it. Accepts
-- 'dlgCacheParam' as input parameter. It's supposed to be passed from
-- configuration.
--
-- * Sets '_dwEpochId' to epoch of tip.
-- * Initializes mempools/LRU caches.
mkDelegationVar ::
       (MonadIO m, MonadBlockDBRead m, HasDlgConfiguration)
    => m DelegationVar
mkDelegationVar = do
    tip <- getTipHeader
    newTVarIO
        DelegationWrap
        { _dwMessageCache = LRU.newLRU (Just dlgCacheParam)
        , _dwProxySKPool = mempty
        , _dwPoolSize = 1 -- approximate size of the empty mempool.
        , _dwTip = headerHash tip
        }
