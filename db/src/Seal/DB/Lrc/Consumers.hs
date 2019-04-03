{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Seal.DB.Lrc.Consumers
       ( allLrcConsumers
       ) where

import           Universum

import           Seal.Chain.Update (BlockVersionData)
import           Seal.DB.Lrc.Consumer (LrcConsumer)
import           Seal.DB.Lrc.Consumer.Delegation (dlgLrcConsumer)
import           Seal.DB.Lrc.Consumer.Ssc (sscLrcConsumer)
import           Seal.DB.Lrc.Consumer.Update (usLrcConsumer)
import           Seal.DB.Lrc.Mode (LrcMode)

allLrcConsumers :: LrcMode ctx m => BlockVersionData -> [LrcConsumer m]
allLrcConsumers genesisBvd =
    ($ genesisBvd) <$> [dlgLrcConsumer, usLrcConsumer, sscLrcConsumer]
