{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Update System types

module Test.Seal.DB.Update.Arbitrary.MemState
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))

import           Seal.Binary.Class (biSize)
import qualified Seal.DB.Update as Upd

import           Test.Seal.Chain.Update.Arbitrary ()
import           Test.Seal.Crypto.Arbitrary ()

instance Arbitrary Upd.MemPool where
    arbitrary = do
        proposals <- arbitrary
        votes <- arbitrary
        return $ Upd.MemPool proposals votes (biSize proposals + biSize votes)
