{-# LANGUAGE TypeApplications #-}

module Test.Seal.Chain.Ssc.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Seal.Chain.Ssc (VssCertificate)

import           Test.Seal.Binary.Helpers (binaryTest)
import           Test.Seal.Chain.Ssc.Arbitrary ()

spec :: Spec
spec = describe "Cbor Bi instances" $ binaryTest @VssCertificate
