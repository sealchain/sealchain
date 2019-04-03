{-# LANGUAGE RankNTypes #-}
module Seal.Core.Common.GoldCoin
       ( GoldCoin (..)
       , maxGoldCoinVal
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

-- | GoldCoin is the least possible unit of currency.
newtype GoldCoin = GoldCoin
    { getGoldCoin :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

deriveSafeCopySimple 0 'base ''GoldCoin

instance Buildable GoldCoin where
    build (GoldCoin n) = bprint (int%" gc(s)") n

instance Bounded GoldCoin where
    minBound = GoldCoin 0
    maxBound = GoldCoin maxGoldCoinVal

-- | Maximal possible value of 'GoldCoin'.
maxGoldCoinVal :: Word64
maxGoldCoinVal = 1000000000000

instance Currency GoldCoin where
    mkMoney c = either error (const gold) (checkMoney gold)
      where
        gold = (GoldCoin c)
    {-# INLINE mkMoney #-}

    checkMoney (GoldCoin c)
        | c <= maxGoldCoinVal = pure ()
        | otherwise       = throwError $ "GoldCoin: " <> show c <> " is too large"

    formatter = build

    unsafeGetMoney = getGoldCoin
    {-# INLINE unsafeGetMoney #-}

instance Default GoldCoin where
    def = GoldCoin 0

instance Bi GoldCoin where
    encode = encode . unsafeGetMoney
    decode = GoldCoin <$> decode

instance Aeson.FromJSON GoldCoin where
    parseJSON v = mkMoney <$> Aeson.parseJSON v

instance Aeson.ToJSON GoldCoin where
    toJSON = Aeson.toJSON . unsafeGetMoney

instance Monad m => Canonical.ToJSON m GoldCoin where
    toJSON = Canonical.toJSON @_ @Word64 . unsafeGetMoney  -- i. e. String

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m GoldCoin where
    fromJSON = fmap GoldCoin . Canonical.fromJSON
