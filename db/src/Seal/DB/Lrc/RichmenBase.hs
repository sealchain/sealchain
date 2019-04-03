{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Richmen part of LRC DB.

module Seal.DB.Lrc.RichmenBase
       (
         -- * Generalization
         RichmenComponent (..)

         -- * Getters
       , getRichmen

       -- * Operations
       , putRichmen
       ) where

import           Universum

import           Seal.Binary.Class (Bi, serialize')
import           Seal.Chain.Lrc (FullRichmenData, RichmenComponent (..))
import           Seal.Core.Slotting (EpochIndex)
import           Seal.DB.Class (MonadDB, MonadDBRead)
import           Seal.DB.Lrc.Common (getBi, putBi)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getRichmen
    :: (Bi richmenData, MonadDBRead m)
    => RichmenComponent richmenData
    -> EpochIndex
    -> m (Maybe richmenData)
getRichmen = getBi ... richmenKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putRichmen
    :: (Bi richmenData, MonadDB m)
    => RichmenComponent richmenData
    -> EpochIndex
    -> FullRichmenData
    -> m ()
putRichmen rc e = putBi (richmenKey rc e) . rcToData rc

richmenKey :: RichmenComponent richmenData -> EpochIndex -> ByteString
richmenKey rc e = mconcat ["r/", rcTag rc, "/", serialize' e]
