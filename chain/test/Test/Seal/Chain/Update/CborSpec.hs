{-# LANGUAGE TypeApplications #-}

module Test.Seal.Chain.Update.CborSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Seal.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), SoftforkRule (..),
                     SoftwareVersion (..))

import           Test.Seal.Binary.Helpers (binaryTest)
import           Test.Seal.Chain.Update.Arbitrary ()

spec :: Spec
spec = describe "Cbor Bi instances" $ do
    binaryTest @ApplicationName
    binaryTest @BlockVersion
    binaryTest @BlockVersionData
    binaryTest @SoftforkRule
    binaryTest @SoftwareVersion
