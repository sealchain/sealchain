-- | Run-time errors in Slotting.

module Seal.Infra.Slotting.Error
       ( SlottingError (..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import           Formatting (bprint, (%))
import qualified Formatting.Buildable

import           Seal.Core.Exception (sealExceptionFromException,
                     sealExceptionToException)
import           Seal.Core.Slotting (SlotId, slotIdF)

-- | Type aggregating run-time errors related to Slotting.
data SlottingError = SEUnknownSlotStart !SlotId
  deriving (Show, Typeable)

instance Buildable SlottingError where
    build (SEUnknownSlotStart slot) =
        bprint ("start of "%slotIdF%" is surprisingly unknown") slot

instance Exception SlottingError where
    toException = sealExceptionToException
    fromException = sealExceptionFromException
    displayException = toString . pretty
