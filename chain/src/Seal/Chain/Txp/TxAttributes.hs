{-# LANGUAGE RecordWildCards #-}

module Seal.Chain.Txp.TxAttributes
       ( SealAttributes (..)
       , AttachmentTag (..)
       , Attachments
       , TxAttributes
       , remarkSealAttributes
       , encodeRemarks
       , decodeRemarks
       ) where

import           Universum
import qualified Universum.Unsafe as Unsafe

import           Data.Aeson (FromJSON (..), ToJSON (toJSON), FromJSONKeyFunction (..),
                    ToJSONKey (..), FromJSONKey (..),
                    object, withObject, (.=), (.:?))
import           Data.Aeson.Types (toJSONKeyText)
import           Data.Default (Default, def)
import qualified Data.Hashable as H
import qualified Data.Map as M
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, build, int, sformat, (%))
import qualified Serokell.Util.Base16 as B16

import           Seal.Binary.Class (Bi, decode, encode)
import qualified Seal.Binary.Class as Bi
import           Seal.Core.Attributes (Attributes (..), decodeAttributes, encodeAttributes)
import           Seal.Util.Util (toAesonError)

data AttachmentTag 
    = Remarks
    | Unknown !Word8
    deriving (Eq, Ord, Generic, Typeable, Show)

instance Hashable AttachmentTag where
    hashWithSalt salt at = H.hashWithSalt salt $ case at of 
        Remarks   -> 0 
        Unknown t -> t

instance Bi AttachmentTag where
    encode =
        encode @Word8 . \case
            Remarks   -> 0
            Unknown t -> t
    decode =
        decode @Word8 <&> \case
            0 -> Remarks
            t -> Unknown t

instance NFData AttachmentTag

deriveSafeCopySimple 0 'base ''AttachmentTag

-- 附件
type Attachments = Map AttachmentTag ByteString

instance Hashable Attachments where
    hashWithSalt salt = H.hashWithSalt salt . M.toList

instance Buildable Attachments where
    build = bprint (build)

data SealAttributes = SealAttributes
    { attachments :: !Attachments
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable SealAttributes where
    build (SealAttributes {..}) =
        bprint
            ("SealAttributes { attachments: "%build%"}")
            attachments

instance NFData SealAttributes
instance Hashable SealAttributes

instance Bi (Attributes SealAttributes) where
    encode attrs@(Attributes {attrData = SealAttributes attas}) =
        encodeAttributes attachmentsWithIndices attrs
      where
        attachmentsWithIndices = 
            if | M.null attas  -> []
               | otherwise     -> [(0, Bi.serialize . attachments)]

    decode = decodeAttributes initValue go
      where
        initValue =
            SealAttributes
            {attachments = M.empty
            }
        go n v acc =
            case n of
                0 -> (\deriv -> Just $ acc {attachments = deriv }) <$> Bi.deserialize v
                _ -> pure Nothing

instance Default SealAttributes where
    def = SealAttributes M.empty

attachmentTagFromText :: Text -> Either Text AttachmentTag
attachmentTagFromText t = case t of
    "0" -> Right Remarks
    _   -> Left $ "Unknown attachmet tag: " <> t -- todo xiaolie how to convert to unknown?

attachmentTagToText :: AttachmentTag -> Text
attachmentTagToText Remarks = "0"
attachmentTagToText (Unknown t) = sformat (int) t

instance FromJSON AttachmentTag where
    parseJSON v = toAesonError =<< attachmentTagFromText <$> parseJSON v

instance ToJSON AttachmentTag where
    toJSON = toJSON . attachmentTagToText

instance FromJSONKey AttachmentTag where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . attachmentTagFromText)

instance ToJSONKey AttachmentTag where
    toJSONKey = toJSONKeyText attachmentTagToText

instance FromJSON SealAttributes where
    parseJSON = withObject "SealAttributes" $ \o -> do
        attachments <- o .:? "attachments" >>= \mm -> do
            case mm of 
                Just mat  -> toAesonError $ convHex2BS mat
                Nothing   -> pure $ M.empty
        return $ SealAttributes attachments
      where
        convHex2BS :: Map AttachmentTag Text -> Either Text (Map AttachmentTag ByteString)
        convHex2BS mat = 
            let (fm, sm) = M.mapEither B16.decode mat
            in if | (M.size fm) > 0 -> Left . snd . Unsafe.head $ M.toList fm
                  | otherwise       -> Right sm

instance ToJSON SealAttributes where
    toJSON (SealAttributes attachments) = object [
            "attachments" .= conved ]
        where
            conved = M.map B16.encode attachments

deriveSafeCopySimple 0 'base ''SealAttributes

encodeRemarks :: Text -> ByteString
encodeRemarks = encodeUtf8

decodeRemarks :: ByteString -> Maybe Text
decodeRemarks bs = case decodeUtf8' bs of 
    Left _  -> Nothing
    Right t -> Just t

remarkSealAttributes :: Maybe Text -> SealAttributes
remarkSealAttributes (Just t) = SealAttributes . M.singleton Remarks $ encodeRemarks t
remarkSealAttributes Nothing = def

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending transaction with new
-- fields via softfork.
type TxAttributes = Attributes SealAttributes
