-- | Types describing runtime errors related to Txp.

module Seal.Chain.Txp.Error
       ( TxpError (..)
       ) where

import           Control.Exception.Safe (Exception (..))
import           Formatting (bprint, stext, (%))
import qualified Formatting.Buildable
import           Universum

import           Seal.Core.Exception (sealExceptionFromException,
                     sealExceptionToException)

data TxpError
    = TxpInternalError !Text
    -- ^ Something bad happened inside Txp
    deriving (Show)

instance Exception TxpError where
    toException = sealExceptionToException
    fromException = sealExceptionFromException
    displayException = toString . pretty

instance Buildable TxpError where
    build (TxpInternalError msg) =
        bprint ("internal error in Transaction processing: "%stext) msg
