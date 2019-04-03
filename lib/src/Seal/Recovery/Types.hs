
module Seal.Recovery.Types
       ( MonadRecoveryHeader
       ) where

import           Universum

import           Seal.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Seal.Util.Util (HasLens (..))

type MonadRecoveryHeader ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx RecoveryHeader)
