{-# LANGUAGE TypeFamilies #-}

module Seal.Infra.Shutdown.Class
       ( HasShutdownContext(..)
       ) where

import           Universum

import           Control.Lens.Lens (lens)
import           Seal.Infra.Shutdown.Types (ShutdownContext)

class HasShutdownContext ctx where
    shutdownContext :: Lens' ctx ShutdownContext

instance HasShutdownContext ShutdownContext where
    shutdownContext = lens identity (\_ x -> x)
