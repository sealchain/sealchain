{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Seal.Cbor.Arbitrary.UserPublic
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Seal.Util.UserPublic (UserPublic, WalletUserPublic)
import           System.FileLock (FileLock)

instance Arbitrary WalletUserPublic where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Maybe FileLock) => Arbitrary UserPublic where
    arbitrary = genericArbitrary
    shrink = genericShrink
