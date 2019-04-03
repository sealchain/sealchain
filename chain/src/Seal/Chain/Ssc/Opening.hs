module Seal.Chain.Ssc.Opening
       ( Opening (..)
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)

import           Seal.Binary.Class (AsBinary, Bi (..))
import           Seal.Crypto (Secret)

-- | Opening reveals secret.
newtype Opening = Opening
    { getOpening :: AsBinary Secret
    } deriving (Show, Eq, Generic, Buildable, NFData)

instance Bi Opening where
    encode = encode . getOpening
    decode = Opening <$> decode

deriveSafeCopySimple 0 'base ''Opening
