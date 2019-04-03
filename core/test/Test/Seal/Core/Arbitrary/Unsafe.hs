{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Arbitrary' unsafe instances for some types from 'Seal.Core.Types'.

module Test.Seal.Core.Arbitrary.Unsafe () where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Seal.Core (AddrCategory (..), AddrAttributes (..), AddrStakeDistribution (..),
                     AddrType (..), Address (..), Coin (..), EpochIndex (..),
                     LocalSlotIndex, SharedSeed (..), SlotId (..))
import           Seal.Core.Attributes (mkAttributes)
import           Seal.Core.NetworkMagic (NetworkMagic (..))

import           Test.Seal.Core.Arbitrary ()
import           Test.Seal.Crypto.Arbitrary ()
import           Test.Seal.Util.QuickCheck.Arbitrary (ArbitraryUnsafe (..))

deriving instance ArbitraryUnsafe SharedSeed
deriving instance ArbitraryUnsafe EpochIndex

instance ArbitraryUnsafe LocalSlotIndex where

instance ArbitraryUnsafe Coin where
    arbitraryUnsafe = mkCoin <$> arbitraryUnsafe

instance ArbitraryUnsafe Address where
    arbitraryUnsafe = do
        addrRoot <- arbitraryUnsafe
        aaC <- arbitraryUnsafe
        let addrAttributes =
                mkAttributes $
                AddrAttributes
                { aaPkDerivationPath = Nothing
                , aaStakeDistribution = BootstrapEraDistr
                , aaCategory = aaC
                }
        let addrType = ATPubKey
        return Address {..}

instance ArbitraryUnsafe SlotId where
    arbitraryUnsafe = SlotId <$> arbitraryUnsafe <*> arbitraryUnsafe

instance ArbitraryUnsafe NetworkMagic

instance ArbitraryUnsafe (Maybe AddrCategory)

-- | Makes a 'Coin' but is _|_ if that coin exceeds 'maxCoinVal'.
-- You can also use 'checkCoin' to do that check.
mkCoin :: Word64 -> Coin
mkCoin c = either error (const coin) (checkCoin coin)
  where
    coin = (Coin c)
{-# INLINE mkCoin #-}

checkCoin :: MonadError Text m => Coin -> m ()
checkCoin (Coin c)
    | c <= maxCoinVal = pure ()
    | otherwise       = throwError $ "Coin: " <> show c <> " is too large"

-- | Maximal possible value of 'Coin'.
maxCoinVal :: Word64
maxCoinVal = 45000000000000000