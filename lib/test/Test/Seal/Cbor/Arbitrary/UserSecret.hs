{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Seal.Cbor.Arbitrary.UserSecret
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Seal.Util.UserSecret (UserSecret, WalletUserSecret)
import           System.FileLock (FileLock)

instance Arbitrary WalletUserSecret where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Maybe FileLock) => Arbitrary UserSecret where
    arbitrary = genericArbitrary
    shrink = genericShrink
