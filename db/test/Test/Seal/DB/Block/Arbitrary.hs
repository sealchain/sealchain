{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Seal.DB.Block.Arbitrary () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary,
                     genericShrink)

import           Seal.Chain.Block (SlogUndo, Undo (..))

import           Test.Seal.Chain.Delegation.Arbitrary ()
import           Test.Seal.Chain.Txp.Arbitrary ()
import           Test.Seal.Core.Arbitrary ()
import           Test.Seal.DB.Update.Arbitrary ()

instance Arbitrary SlogUndo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Undo where
    arbitrary = genericArbitrary
    shrink = genericShrink
