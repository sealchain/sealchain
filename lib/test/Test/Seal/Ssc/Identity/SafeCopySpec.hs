-- | This module tests SafeCopy instances.

module Test.Seal.Ssc.Identity.SafeCopySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import qualified Seal.Chain.Ssc as Ssc

import           Test.Seal.Binary.Helpers (safeCopyTest)
import           Test.Seal.Infra.Arbitrary.Ssc ()

spec :: Spec
spec = describe "Ssc" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @Ssc.Commitment
        safeCopyTest @Ssc.CommitmentSignature
        safeCopyTest @Ssc.SignedCommitment
        safeCopyTest @Ssc.Opening
        safeCopyTest @Ssc.SscPayload
        safeCopyTest @Ssc.SscProof
