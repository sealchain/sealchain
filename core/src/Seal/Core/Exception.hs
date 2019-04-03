{-# LANGUAGE ExistentialQuantification #-}

-- | Exceptions hierarchy in seal.

module Seal.Core.Exception
       ( SealException (..)
       , sealExceptionToException
       , sealExceptionFromException

       , SealFatalError (..)
       , reportFatalError
       , assertionFailed
       ) where

import           Control.Exception.Safe (Exception (..))
import           Data.Typeable (cast)
import           Formatting (bprint, stext, (%))
import qualified Formatting.Buildable
import           Seal.Util.Wlog (WithLogger, logError)
import           Serokell.Util (Color (Red), colorize)
import qualified Text.Show
import           Universum

-- | Root of exceptions in seal.
data SealException =
    forall e. (Buildable e, Exception e) =>
              SealException e
    deriving (Typeable)

instance Show SealException where
    show (SealException e) = toString . pretty $ e

instance Exception SealException

instance Buildable SealException where
    build (SealException e) = Formatting.Buildable.build e

-- | Helper to define sub-exception of SealException.
sealExceptionToException :: (Buildable e, Exception e) => e -> SomeException
sealExceptionToException = toException . SealException

-- | Helper to define sub-exception of SealException.
sealExceptionFromException :: Exception e => SomeException -> Maybe e
sealExceptionFromException x = do
    SealException a <- fromException x
    cast a


-- | Error indicating that something really bad happened. Should be
-- used when serious assertions fail (local equivalent of
-- 'panic'). 'panic' is still alright to use, but preferably in pure
-- environment.
data SealFatalError =
    SealFatalError !Text
    deriving (Typeable, Show)

instance Buildable SealFatalError where
    build (SealFatalError msg) =
        bprint ("Seal fatal error: "%stext) msg

instance Exception SealFatalError where
    toException = sealExceptionToException
    fromException = sealExceptionFromException
    displayException = toString . pretty

-- | Print red message about fatal error and throw exception.
reportFatalError
    :: (WithLogger m, MonadThrow m)
    => Text -> m a
reportFatalError msg = do
    logError $ colorize Red msg
    throwM $ SealFatalError msg

-- | Report 'SealFatalError' for failed assertions.
assertionFailed :: (WithLogger m, MonadThrow m) => Text -> m a
assertionFailed msg =
    reportFatalError $ "assertion failed: " <> msg
