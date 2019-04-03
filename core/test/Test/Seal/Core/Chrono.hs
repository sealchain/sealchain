{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Chronological sequences.

module Test.Seal.Core.Chrono
       (
       ) where

import           Seal.Core.Chrono

import           Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (f a) => Arbitrary (NewestFirst f a)
deriving instance Arbitrary (f a) => Arbitrary (OldestFirst f a)
