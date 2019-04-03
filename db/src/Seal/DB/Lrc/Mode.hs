{-# LANGUAGE DataKinds #-}

-- | Constraints for LRC; a restricted version of `WorkMode`.

module Seal.DB.Lrc.Mode
       ( LrcMode
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Seal.DB.Class (MonadDB, MonadGState)
import           Seal.DB.Lrc.Context (HasLrcContext)
import           Seal.Util.Wlog (WithLogger)

-- | Set of constraints used by LRC.
type LrcMode ctx m
     = ( WithLogger m
       , MonadMask m
       , MonadGState m
       , MonadDB m
       , MonadIO m
       , MonadUnliftIO m
       , MonadReader ctx m
       , HasLrcContext ctx
       )
