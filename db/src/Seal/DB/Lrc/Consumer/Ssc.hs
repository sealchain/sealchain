{-# LANGUAGE TypeFamilies #-}

-- | Richmen computation for SSC.

module Seal.DB.Lrc.Consumer.Ssc
       (
       -- * The 'RichmenComponent' instance
         sscRichmenComponent

       -- * The consumer
       , sscLrcConsumer

       -- * Functions for getting richmen
       , getSscRichmen
       , tryGetSscRichmen
       ) where

import           Universum

import           Seal.Chain.Lrc (RichmenComponent (..), RichmenStakes)
import           Seal.Chain.Update (BlockVersionData (..))
import           Seal.Core (EpochIndex)
import           Seal.DB (MonadDB, MonadDBRead, MonadGState)
import           Seal.DB.Lrc.Consumer (LrcConsumer,
                     lrcConsumerFromComponentSimple)
import           Seal.DB.Lrc.Context (HasLrcContext, lrcActionOnEpochReason)
import           Seal.DB.Lrc.RichmenBase (getRichmen)

----------------------------------------------------------------------------
-- RichmenComponent
----------------------------------------------------------------------------

sscRichmenComponent :: BlockVersionData -> RichmenComponent RichmenStakes
sscRichmenComponent genesisBvd = RichmenComponent
    { rcToData            = snd
    , rcTag               = "ssc"
    , rcInitialThreshold  = bvdMpcThd genesisBvd
    , rcConsiderDelegated = True
    }

----------------------------------------------------------------------------
-- The consumer
----------------------------------------------------------------------------

-- | Consumer will be called on every Richmen computation.
sscLrcConsumer :: (MonadGState m, MonadDB m) => BlockVersionData -> LrcConsumer m
sscLrcConsumer genesisBvd =
    lrcConsumerFromComponentSimple (sscRichmenComponent genesisBvd) bvdMpcThd

----------------------------------------------------------------------------
-- Getting richmen
----------------------------------------------------------------------------

-- | Wait for LRC results to become available and then get the list of SSC
-- ricmen for the given epoch.
getSscRichmen
    :: (MonadIO m, MonadDBRead m, MonadReader ctx m, HasLrcContext ctx)
    => BlockVersionData
    -> Text               -- ^ Function name (to include into error message)
    -> EpochIndex         -- ^ Epoch for which you want to know the richmen
    -> m RichmenStakes
getSscRichmen genesisBvd fname epoch = lrcActionOnEpochReason
    epoch
    (fname <> ": couldn't get SSC richmen")
    (tryGetSscRichmen genesisBvd)

-- | Like 'getSscRichmen', but doesn't wait and doesn't fail.
tryGetSscRichmen
    :: MonadDBRead m
    => BlockVersionData
    -> EpochIndex
    -> m (Maybe RichmenStakes)
tryGetSscRichmen = getRichmen . sscRichmenComponent
