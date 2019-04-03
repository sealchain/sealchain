module Seal.Chain.Genesis.Issuers
       ( Issuers (..)
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson)
import           Text.JSON.Canonical (FromJSON (..), ToJSON (..))

import           Seal.Core.Common (Address)
import           Seal.Util.Json.Canonical (SchemaError)

-- | Owners of the addresses have the permission to issue/destroy golds coin and gold dollars
newtype Issuers = Issuers
    { getIssuers :: [Address]
    } deriving (Show, Eq)

instance Buildable Issuers where
    build (Issuers m) =
        bprint ("Issuers: " %listJson) m

instance Monad m => ToJSON m Issuers where
    toJSON = toJSON . getIssuers

instance MonadError SchemaError m => FromJSON m Issuers where
    fromJSON = fmap Issuers . fromJSON
