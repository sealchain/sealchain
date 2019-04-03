{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' instances for 'Seal.Infra.Communication' types defined in 'src'

module Test.Seal.Infra.Arbitrary.Communication () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericShrink)

import           Seal.Chain.Txp (TxMsgContents (..))
import           Seal.Infra.Communication.Types.Relay (DataMsg (..))

import           Test.Seal.Infra.Arbitrary.Txp ()

instance Arbitrary (DataMsg TxMsgContents) where
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
