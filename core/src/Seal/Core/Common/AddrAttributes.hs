{-# LANGUAGE RecordWildCards #-}

module Seal.Core.Common.AddrAttributes
       ( AddrAttributes (..)
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Serialize as Cereal
import           Formatting (bprint, build, builder, (%))
import qualified Formatting.Buildable as Buildable

import           Seal.Binary.Class (Bi, decode, encode)
import qualified Seal.Binary.Class as Bi
import           Seal.Core.Attributes (Attributes (..), decodeAttributes,
                     encodeAttributes, mkAttributes)
import           Seal.Crypto.HD (HDAddressPayload)

import           Seal.Core.Common.AddrStakeDistribution
import           Seal.Core.Common.AddrCategory

-- | Additional information stored along with address. It's intended
-- to be put into 'Attributes' data type to make it extensible with
-- softfork.
data AddrAttributes = AddrAttributes
    { aaPkDerivationPath  :: !(Maybe HDAddressPayload)
    , aaStakeDistribution :: !AddrStakeDistribution
    , aaCategory          :: !(Maybe AddrCategory)
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable AddrAttributes where
    build (AddrAttributes {..}) =
        case aaCategory of
            Nothing       ->
                bprint
                    ("AddrAttributes { stake distribution: "%build%
                    ", derivation path: "%builder%
                    ", category: normal }")
                    aaStakeDistribution
                    derivationPathBuilder
            Just category ->
                bprint
                    ("AddrAttributes { stake distribution: "%build%
                    ", derivation path: "%builder%
                    ", category: "%build%" }")
                    aaStakeDistribution
                    derivationPathBuilder
                    category
      where
        derivationPathBuilder =
            case aaPkDerivationPath of
                Nothing -> "{}"
                Just _  -> "{path is encrypted}"

instance NFData AddrAttributes

{- NOTE: Address attributes serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'Attributes' are conceptually a map, where keys are numbers ('Word8').

For address there are two attributes:
• 0 — stake distribution, defaults to 'BootstrapEraDistr';
• 1 — derivation path, defaults to 'Nothing'.

-}

instance Bi (Attributes AddrAttributes) where
    -- FIXME @avieth it was observed that for a 150kb block, this call to
    -- encodeAttributes allocated 3.685mb
    -- Try using serialize rather than serialize', to avoid the
    -- toStrict call.
    -- Also consider using a custom builder strategy; serialized attributes are
    -- probably small, right?
    encode attrs@(Attributes {attrData = AddrAttributes derivationPath stakeDistr category}) =
        encodeAttributes listWithIndices attrs
      where
        listWithIndices :: [(Word8, AddrAttributes -> LBS.ByteString)]
        listWithIndices =
            stakeDistributionListWithIndices <> derivationPathListWithIndices <> categoryListWithIndices
        stakeDistributionListWithIndices =
            case stakeDistr of
                BootstrapEraDistr -> []
                _                 -> [(0, Bi.serialize . aaStakeDistribution)]
        derivationPathListWithIndices =
            case derivationPath of
                Nothing -> []
                -- 'unsafeFromJust' is safe, because 'case' ensures
                -- that derivation path is 'Just'.
                Just _ ->
                    [(1, Bi.serialize . unsafeFromJust . aaPkDerivationPath)]
        categoryListWithIndices =
            case category of
                Nothing -> []
                Just _ ->
                    [(2, Bi.serialize . unsafeFromJust . aaCategory)]
        unsafeFromJust =
            fromMaybe
                (error "Maybe was Nothing in Bi (Attributes AddrAttributes)")

    decode = decodeAttributes initValue go
      where
        initValue =
            AddrAttributes
            { aaPkDerivationPath = Nothing
            , aaStakeDistribution = BootstrapEraDistr
            , aaCategory = Nothing
            }
        go n v acc =
            case n of
                0 -> (\distr -> Just $ acc {aaStakeDistribution = distr }    ) <$> Bi.deserialize v
                1 -> (\deriv -> Just $ acc {aaPkDerivationPath = Just deriv }) <$> Bi.deserialize v
                2 -> (\category -> Just $ acc {aaCategory = Just category }) <$> Bi.deserialize v
                _ -> pure Nothing

instance SafeCopy AddrAttributes where
    -- Since there is only a Bi instance for (Attributes AddrAttributes),
    -- we wrap our AddrAttributes before we serialize it.
    putCopy aa = contain $ do
        let bs = Bi.serialize (mkAttributes aa)
        safePut bs
     -- Try decoding as a BSL.ByteString containing the new format, but if that
    -- fails go for the legacy format.
    getCopy = contain $ label $ getNonLegacy <|> getLegacy
      where
        label = Cereal.label "Seal.Core.Common.AddrAttributes.AddrAttributes:"
        --
        getNonLegacy = do
            eAAA <- Bi.decodeFull <$> safeGet
            case eAAA of
                Left  err -> fail (show err)
                Right aaa -> pure (attrData aaa)
        --
        getLegacy = AddrAttributes <$> safeGet
                                   <*> safeGet
                                   <*> safeGet
