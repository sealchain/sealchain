-- | This module tests SafeCopy instances.

module Test.Seal.Update.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import qualified Seal.Chain.Update as U

import           Test.Seal.Binary.Helpers (safeCopyTest)
import           Test.Seal.Chain.Update.Arbitrary ()

spec :: Spec
spec = describe "Update system" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @U.UpdateProposal
        safeCopyTest @U.UpdateVote
        safeCopyTest @U.UpdateData
        safeCopyTest @U.SystemTag
