{-# LANGUAGE Rank2Types #-}

module Seal.Chain.Genesis.Hash
       ( GenesisHash (..)
       ) where

import           Seal.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }
