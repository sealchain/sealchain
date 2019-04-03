{-# OPTIONS_GHC -fno-warn-orphans #-}

module Seal.Core.Common.StakeholderId
       ( StakeholderId
       ) where

import           Universum

import           Formatting (formatToString)
import           Text.JSON.Canonical (FromObjectKey (..), JSValue (..),
                     ReportSchemaErrors, ToObjectKey (..))

import           Seal.Core.Common.AddressHash
import           Seal.Crypto (decodeAbstractHash, hashHexF)
import           Seal.Crypto.Signing (PublicKey)
import           Seal.Util.Json.Canonical ()
import           Seal.Util.Json.Parse (tryParseString)


-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . formatToString hashHexF

instance ReportSchemaErrors m => FromObjectKey m StakeholderId where
    fromObjectKey = fmap Just . tryParseString (decodeAbstractHash) . JSString
