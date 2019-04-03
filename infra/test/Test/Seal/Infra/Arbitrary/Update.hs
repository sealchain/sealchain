{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System networking types.

module Test.Seal.Infra.Arbitrary.Update
       (
       ) where

import           Universum hiding (id)

import           Test.QuickCheck (Arbitrary (..), listOf)

import           Seal.Chain.Update (UpdateProposal (..), UpdateVote (..),
                     mkUpdateVote)
import           Seal.Crypto (hash)
import           Seal.Infra.Communication.Relay (DataMsg (..))

import           Test.Seal.Chain.Update.Arbitrary ()
import           Test.Seal.Core.Arbitrary ()
import           Test.Seal.Crypto.Dummy (dummyProtocolMagic)

instance Arbitrary (DataMsg UpdateVote) where
    arbitrary = DataMsg <$> arbitrary

instance Arbitrary (DataMsg (UpdateProposal, [UpdateVote])) where
    arbitrary = do
        up <- arbitrary
        let id = hash up
            genVote = mkUpdateVote dummyProtocolMagic <$> arbitrary <*> pure id <*> arbitrary
        votes <- listOf genVote
        pure $ DataMsg (up, votes)

-- TODO [CSL-859]
-- FYI: difficulty now is that 'updateVoteNumLimit' is not constant.
-- instance Arbitrary (MaxSize (DataMsg (UpdateProposal, [UpdateVote]))) where
--     arbitrary =
--         -- we don't care about sensibility
--         MaxSize . DataMsg <$> ((,) <$> arbitrary <*> vector updateVoteNumLimit)
