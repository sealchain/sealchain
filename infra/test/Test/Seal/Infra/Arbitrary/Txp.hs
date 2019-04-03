{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Seal.Infra.Communication' types defined in 'src'

module Test.Seal.Infra.Arbitrary.Txp () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Seal.Chain.Txp (TxMsgContents (..))

import           Test.Seal.Chain.Txp.Arbitrary ()

instance Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink
