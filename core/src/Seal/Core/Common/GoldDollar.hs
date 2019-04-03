{-# LANGUAGE RankNTypes #-}
module Seal.Core.Common.GoldDollar
       ( GoldDollar (..)
       , maxGoldDollarVal
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (toJSON))
import           Data.Data (Data)
import           Data.Default (Default (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable
import qualified Text.JSON.Canonical as Canonical (FromJSON (..),
                     ReportSchemaErrors, ToJSON (..))

import           Seal.Binary.Class (Bi (..))
import           Seal.Core.Common.Currency (Currency (..))
import           Seal.Util.Json.Canonical ()

-- | GoldDollar is the least possible unit of currency.
newtype GoldDollar = GoldDollar
    { getGoldDollar :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

deriveSafeCopySimple 0 'base ''GoldDollar

instance Buildable GoldDollar where
    build (GoldDollar n) = bprint (int%" gd(s)") n

instance Bounded GoldDollar where
    minBound = GoldDollar 0
    maxBound = GoldDollar maxGoldDollarVal

-- | Maximal possible value of 'GoldDollar'.
maxGoldDollarVal :: Word64
maxGoldDollarVal = 1000000000000000

instance Currency GoldDollar where
    mkMoney c = either error (const dollar) (checkMoney dollar)
      where
        dollar = (GoldDollar c)
    {-# INLINE mkMoney #-}

    checkMoney (GoldDollar c)
        | c <= maxGoldDollarVal = pure ()
        | otherwise       = throwError $ "GoldDollar: " <> show c <> " is too large"

    formatter = build

    unsafeGetMoney = getGoldDollar
    {-# INLINE unsafeGetMoney #-}

instance Default GoldDollar where
    def = GoldDollar 0

instance Bi GoldDollar where
    encode = encode . unsafeGetMoney
    decode = GoldDollar <$> decode

instance Aeson.FromJSON GoldDollar where
    parseJSON v = mkMoney <$> Aeson.parseJSON v

instance Aeson.ToJSON GoldDollar where
    toJSON = Aeson.toJSON . unsafeGetMoney

instance Monad m => Canonical.ToJSON m GoldDollar where
    toJSON = Canonical.toJSON @_ @Word64 . unsafeGetMoney  -- i. e. String

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m GoldDollar where
    fromJSON = fmap GoldDollar . Canonical.fromJSON