{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from Txp types

module Test.Seal.Chain.Txp.Arbitrary.Unsafe () where

import           Universum

import           Seal.Chain.Txp (TxOut (..))

import           Test.Seal.Core.Arbitrary.Unsafe ()
import           Test.Seal.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

instance ArbitraryUnsafe TxOut where
    arbitraryUnsafe = TxOut <$> arbitraryUnsafe <*> arbitraryUnsafe
