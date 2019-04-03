{-# LANGUAGE TemplateHaskell #-}

module Test.Seal.Chain.Update.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Seal.Chain.Update.Gen (genBlockVersionData,
                     genSoftforkRule)
import           Test.Seal.Util.Golden (eachOf)
import           Test.Seal.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonBuildable)

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
