{-# LANGUAGE TypeFamilies #-}

-- | Base database Stakes operations.

module Seal.DB.GState.Stakes
       ( StakeIter
         -- * Getters
       , getRealTotalStake
       , getRealStake
       , getRealStakeSumMaybe

       , ftsStakeKey
       , ftsSumKey
       ) where

import           Universum

import           Seal.Core.Common (Coin, StakeholderId)
import           Seal.DB.Class (DBIteratorClass (..), MonadDBRead)
import           Seal.DB.Error (DBError (DBMalformed))
import           Seal.DB.Functions (encodeWithKeyPrefix)
import           Seal.DB.GState.Common (gsGetBi)
import           Seal.Util.Util (maybeThrow)


data StakeIter

instance DBIteratorClass StakeIter where
    type IterKey StakeIter = StakeholderId
    type IterValue StakeIter = Coin
    iterKeyPrefix = iterationPrefix

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

iterationPrefix :: ByteString
iterationPrefix = "b/s/"

ftsStakeKey :: StakeholderId -> ByteString
ftsStakeKey = encodeWithKeyPrefix @StakeIter

ftsSumKey :: ByteString
ftsSumKey = "b/ftssum"

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

-- | Get real total amount of stake to be used for follow-the-satoshi
-- and other procedures which use stake of some stakeholder.
getRealTotalStake :: MonadDBRead m => m Coin
getRealTotalStake =
    maybeThrow (DBMalformed "no total FTS stake in GState DB") =<< getRealStakeSumMaybe

-- | Get real stake owned by given stakeholder (according to rules
-- used for FTS and similar procedures).
getRealStake :: MonadDBRead m => StakeholderId -> m (Maybe Coin)
getRealStake = gsGetBi . ftsStakeKey

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

getRealStakeSumMaybe :: MonadDBRead m => m (Maybe Coin)
getRealStakeSumMaybe = gsGetBi ftsSumKey
