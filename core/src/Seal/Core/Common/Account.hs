module Seal.Core.Common.Account 
( Account (..)
, fromPublicKey
, isTextAccount
, decodeTextAccount
, AccountModifier
) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (toJSON))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, sformat, build, (%))

import           Seal.Binary.Class (Bi (..), cborError)
import           Seal.Crypto.Signing (PublicKey)
import           Seal.Core.Attributes (attrData)
import           Seal.Core.Common.AccountState
import           Seal.Core.Common.AddrAttributes
import           Seal.Core.Common.AddrSpendingData
import           Seal.Core.Common.AddrStakeDistribution
import           Seal.Core.Common.AddrCategory
import           Seal.Core.Common.Address
import qualified Seal.Util.Modifier as MM
import           Seal.Util.Util (toAesonError)

newtype Account 
  = Account
  { getAccount :: Address
  } deriving (Eq, Ord, Generic, Show, Typeable)

instance Hashable Account
instance NFData Account

deriveSafeCopySimple 0 'base ''Account

instance Buildable Account where
    build (Account addr) = bprint build addr

instance Bi Account where
    encode = encode . getAccount
    decode = do
        addr <- decode
        case (aaCategory . attrData $ addrAttributes addr) of 
          Just ACUserAccount -> pure $ Account addr
          actualCategory     -> do
            let errorFmt = "Decoding Account, expected category " % build % " was not the actual one, which was " % build
            cborError (sformat errorFmt (Just ACUserAccount) actualCategory)

instance FromJSON Account where
    parseJSON = toAesonError . decodeTextAccount <=< parseJSON

instance ToJSON Account where
    toJSON (Account addr) = toJSON $ sformat addressF addr

fromPublicKey :: PublicKey -> Account
fromPublicKey key =
    Account $ makeAddress spendingData attrs
  where
    spendingData = PubKeyASD key
    distr = BootstrapEraDistr
    attrs = AddrAttributes {aaStakeDistribution = distr, aaPkDerivationPath = Nothing, aaCategory = Just ACUserAccount}

isTextAccount :: Text -> Bool
isTextAccount text = case (decodeTextAddress text) of
    Left _     -> False
    Right addr -> (aaCategory . attrData $ addrAttributes addr) == (Just ACUserAccount)

decodeTextAccount :: Text -> Either Text Account
decodeTextAccount textAcct = case (decodeTextAddress textAcct) of
    Left msg     -> Left msg
    Right addr   -> case (aaCategory . attrData $ addrAttributes addr) of
      Just ACUserAccount -> Right $ Account addr
      _                  -> Left "Not an Account"

----------------------------------------------------------------------------
-- AccountModifier
----------------------------------------------------------------------------

type AccountModifier = MM.MapModifier Account AccountState
