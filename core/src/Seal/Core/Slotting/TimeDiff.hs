{-# LANGUAGE RecordWildCards #-}

module Seal.Core.Slotting.TimeDiff
       ( TimeDiff (..)
       , addTimeDiffToTimestamp
       , subTimeDiffSafe
       ) where

import           Universum

import           Data.Aeson (Value (..))
import           Data.HashMap.Strict (singleton)
import           Data.Time.Units (Microsecond, toMicroseconds)
import qualified Formatting.Buildable as Buildable
import qualified Prelude

import           Seal.Binary.Class (Bi (..))
import           Seal.Core.Slotting.Timestamp
import           Seal.Util.Log (ToObject (..))

-- | Difference between two timestamps
newtype TimeDiff = TimeDiff
    { getTimeDiff :: Microsecond
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show TimeDiff where
    show = show . toInteger . getTimeDiff

instance Read TimeDiff where
    readsPrec i = fmap (first (TimeDiff . fromInteger)) . Prelude.readsPrec i

instance Buildable TimeDiff where
    build = Buildable.build . toInteger

instance Bi TimeDiff where
    encode = encode . toInteger
    decode = fromInteger <$> decode

instance NFData TimeDiff where
    rnf TimeDiff{..} = rnf (toInteger getTimeDiff)

instance ToObject TimeDiff where
    toObject (TimeDiff usec) = singleton "TimeDiff" $ String $ show $ toMicroseconds usec

addTimeDiffToTimestamp :: TimeDiff -> Timestamp -> Timestamp
addTimeDiffToTimestamp = addMicrosecondsToTimestamp . getTimeDiff

subTimeDiffSafe :: TimeDiff -> TimeDiff -> TimeDiff
subTimeDiffSafe (TimeDiff t1) (TimeDiff t2)
    | t1 >= t2  = TimeDiff (t1 - t2)
    | otherwise = error "subTimeDiffSafe: first TimeDiff must be more or equal second TimeDiff"
