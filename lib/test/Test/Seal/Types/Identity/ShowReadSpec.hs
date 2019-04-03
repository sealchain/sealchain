-- | This module tests Show/Read instances.

module Test.Seal.Types.Identity.ShowReadSpec
       ( spec
       ) where

import           Universum

import           Seal.Core (Timestamp (..))
import           Test.Hspec (Spec, describe)

import           Test.Seal.Binary.Helpers (showReadTest)
import           Test.Seal.Core.Arbitrary ()

spec :: Spec
spec = describe "Types" $ do
    describe "Show/Read instances" $ do
        showReadTest @Timestamp
