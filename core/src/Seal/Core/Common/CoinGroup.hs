{-# LANGUAGE RecordWildCards #-}

module Seal.Core.Common.CoinGroup
       ( CoinGroup (..)
       , sumCoinGroups
       , unsafeAddCoinGroup
       , unsafeSubCoinGroup
       , anyGreatThanCoinGroup
       , allLittleEqualCoinGroup
       , maxCoinGroupVal
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (toJSON), 
                    object, withObject, (.:?), (.=))
import           Data.Default (Default (def))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Seal.Binary.Class (Bi (..), 
                    matchSize, encodeListLen, decodeListLenCanonical)
import           Seal.Core.Common.Coin
import           Seal.Core.Common.Currency
import           Seal.Core.Common.GoldCoin
import           Seal.Core.Common.GoldDollar


data CoinGroup = CoinGroup
    { cgSeal   :: Coin
    , cgGold   :: GoldCoin
    , cgDollar :: GoldDollar
    } deriving (Show, Ord, Generic, Eq)

instance Hashable CoinGroup

instance Bounded CoinGroup where
    minBound = CoinGroup (mkMoney 0) (mkMoney 0) (mkMoney 0)
    maxBound = CoinGroup (maxBound::Coin) (maxBound::GoldCoin) (maxBound::GoldDollar)

instance NFData CoinGroup

instance Buildable CoinGroup where
    build CoinGroup {..} =
        bprint ("CoinGroup: "%build%", "%build%", "%build) cgSeal cgGold cgDollar

instance Default CoinGroup where
    def = CoinGroup def def def

instance Bi CoinGroup where
    encode c@CoinGroup{..}
      | c == def = encodeListLen 0
      | otherwise =
          encodeListLen 3
          <> encode cgSeal
          <> encode cgGold
          <> encode cgDollar

    decode = do
        len <- decodeListLenCanonical
        if | len == 0 -> return def
           | otherwise -> do
               matchSize len "CoinGroup" 3
               cgSeal <- decode
               cgGold <- decode
               cgDollar <- decode
               return CoinGroup{..}

instance ToJSON CoinGroup where
    toJSON CoinGroup{..} = object [
        "seal" .= moneyToInteger cgSeal,
        "gold" .= moneyToInteger cgGold,
        "dollar" .= moneyToInteger cgDollar ]

instance FromJSON CoinGroup where
    parseJSON = withObject "CoinGroup" $ \o ->
        CoinGroup <$> ((mkMoney . fromMaybe 0) <$> o .:? "seal")
                  <*> ((mkMoney . fromMaybe 0) <$> o .:? "gold")
                  <*> ((mkMoney . fromMaybe 0) <$> o .:? "dollar")

deriveSafeCopySimple 0 'base ''CoinGroup

instance Semigroup CoinGroup where
    (<>) = unsafeAddCoinGroup

instance Monoid CoinGroup where
    mempty = def
    mappend = (<>)
 
sumCoinGroups :: [CoinGroup] -> CoinGroup
sumCoinGroups = mconcat

unsafeAddCoinGroup :: CoinGroup -> CoinGroup -> CoinGroup
unsafeAddCoinGroup (CoinGroup s1 g1 d1) (CoinGroup s2 g2 d2) =
    CoinGroup {..}
  where
    cgSeal   = unsafeAddMoney s1 s2
    cgGold   = unsafeAddMoney g1 g2
    cgDollar = unsafeAddMoney d1 d2

unsafeSubCoinGroup :: CoinGroup -> CoinGroup -> CoinGroup
unsafeSubCoinGroup (CoinGroup s1 g1 d1) (CoinGroup s2 g2 d2) =
    CoinGroup {..}
    where
    cgSeal   = unsafeSubMoney s1 s2
    cgGold   = unsafeSubMoney g1 g2
    cgDollar = unsafeSubMoney d1 d2

anyGreatThanCoinGroup :: CoinGroup -> CoinGroup -> Bool
anyGreatThanCoinGroup cg1 cg2 =
    cgSeal cg1   > cgSeal cg2 ||
    cgGold cg1   > cgGold cg2 ||
    cgDollar cg1 > cgDollar cg2

allLittleEqualCoinGroup :: CoinGroup -> CoinGroup -> Bool
allLittleEqualCoinGroup cg1 cg2 =
    cgSeal cg1   <= cgSeal cg2 &&
    cgGold cg1   <= cgGold cg2 &&
    cgDollar cg1 <= cgDollar cg2

maxCoinGroupVal :: CoinGroup
maxCoinGroupVal = maxBound
