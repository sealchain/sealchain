module Seal.Core.Common.AddrSpendingData
       ( AddrSpendingData (..)
       , AddrType (..)
       , addrSpendingDataToType
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable as Buildable

import           Seal.Binary.Class (Bi (..), Case (..), szCases)
import qualified Seal.Binary.Class as Bi
import           Seal.Crypto.Signing (PublicKey, RedeemPublicKey)

-- | Data which is bound to an address and must be revealed in order
-- to spend coins belonging to this address.
data AddrSpendingData
    = PubKeyASD !PublicKey
    -- ^ Funds can be spent by revealing a 'PublicKey' and providing a
    -- valid signature.
    | RedeemASD !RedeemPublicKey
    -- ^ Funds can be spent by revealing a 'RedeemPublicKey' and providing a
    -- valid signature.
    | UnknownASD !Word8 !ByteString
    -- ^ Unknown type of spending data. It consists of a tag and
    -- arbitrary 'ByteString'. It allows us to introduce a new type of
    -- spending data via softfork.
    deriving (Eq, Generic, Typeable, Show)

instance Buildable AddrSpendingData where
    build =
        \case
            PubKeyASD pk -> bprint ("PubKeyASD " %build) pk
            RedeemASD rpk -> bprint ("RedeemASD "%build) rpk
            UnknownASD tag _ -> bprint ("UnknownASD with tag "%int) tag

instance NFData AddrSpendingData

{- NOTE: Address spending data serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is serialized as a tuple consisting of:

1. One-byte tag.
2. Data dependent on tag.

If tag is 0 or 1, the type of spending data is 'PubKeyASD' or 
'RedeemASD' respectively.

If tag is greater than 2, the data is decoded as a plain 'ByteString'.

This lets us have backwards compatibility. For instance, if a newer
version of CSL adds a new type of spending data with tag 3, then older
versions would deserialize it as follows:

    UnknownASD 3 <some bytes>
-}

-- Helper function to avoid writing `:: Word8`.
w8 :: Word8 -> Word8
w8 = identity
{-# INLINE w8 #-}

instance Bi AddrSpendingData where
    encode =
        \case
            PubKeyASD pk -> encode (w8 0, pk)
            RedeemASD redeemPK -> encode (w8 1, redeemPK)
            UnknownASD tag payload ->
                -- `encodeListLen 2` is semantically equivalent to encode (x,y)
                -- but we need to "unroll" it in order to apply CBOR's tag 24 to `payload`.
                Bi.encodeListLen 2
                    <> encode tag
                    <> Bi.encodeUnknownCborDataItem (LBS.fromStrict payload)
    decode = do
        Bi.enforceSize "AddrSpendingData" 2
        decode @Word8 >>= \case
            0 -> PubKeyASD <$> decode
            1 -> RedeemASD <$> decode
            tag -> UnknownASD tag <$> Bi.decodeUnknownCborDataItem

    encodedSizeExpr size _ = szCases
        [ let PubKeyASD pk = error "unused"
          in  Case "PubKeyASD" $ size ((,) <$> pure (w8 0) <*> pure pk)
        , let RedeemASD redeemPK = error "unused"
          in  Case "RedeemASD" $ size ((,) <$> pure (w8 1) <*> pure redeemPK)
        ]

-- | Type of an address. It corresponds to constructors of
-- 'AddrSpendingData'. It's separated, because 'Address' doesn't store
-- 'AddrSpendingData', but we want to know its type.
data AddrType
    = ATPubKey
    | ATRedeem
    | ATUnknown !Word8
    deriving (Eq, Ord, Generic, Typeable, Show)

instance NFData AddrType

instance Bi AddrType where
    encode =
        encode @Word8 . \case
            ATPubKey -> 0
            ATRedeem -> 1
            ATUnknown tag -> tag
    decode =
        decode @Word8 <&> \case
            0 -> ATPubKey
            1 -> ATRedeem
            tag -> ATUnknown tag
    encodedSizeExpr size _ = encodedSizeExpr size (Proxy @Word8)

-- | Convert 'AddrSpendingData' to the corresponding 'AddrType'.
addrSpendingDataToType :: AddrSpendingData -> AddrType
addrSpendingDataToType =
    \case
        PubKeyASD {} -> ATPubKey
        RedeemASD {} -> ATRedeem
        UnknownASD tag _ -> ATUnknown tag


-- Define these at the end of the file to avoid TH staging issues.
deriveSafeCopySimple 0 'base ''AddrSpendingData
deriveSafeCopySimple 0 'base ''AddrType -- ☃


