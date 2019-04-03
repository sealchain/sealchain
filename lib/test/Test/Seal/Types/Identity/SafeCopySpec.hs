-- | This module tests SafeCopy instances.

module Test.Seal.Types.Identity.SafeCopySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import qualified Seal.Chain.Ssc as Ssc
import qualified Seal.Chain.Txp as Txp
import qualified Seal.Core as Core

import           Test.Seal.Binary.Helpers (safeCopyTest)
import           Test.Seal.Chain.Ssc.Arbitrary ()
import           Test.Seal.Chain.Txp.Arbitrary ()
import           Test.Seal.Infra.Arbitrary.Txp ()

spec :: Spec
spec = describe "Types" $ do
    describe "SafeCopy instances" $ do
        safeCopyTest @Core.EpochIndex
        safeCopyTest @Core.LocalSlotIndex
        safeCopyTest @Core.SlotId
        safeCopyTest @Core.Coin
        safeCopyTest @Core.Address
        safeCopyTest @Core.Address'
        safeCopyTest @Core.SharedSeed
        safeCopyTest @Core.ChainDifficulty
        safeCopyTest @Ssc.VssCertificate

        safeCopyTest @Txp.TxInWitness
        safeCopyTest @Txp.TxIn
        safeCopyTest @Txp.TxOut
        safeCopyTest @Txp.Tx
