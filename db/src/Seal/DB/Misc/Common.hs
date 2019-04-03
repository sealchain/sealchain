{-# LANGUAGE RankNTypes #-}

-- | Common functionality related to Misc DB.

module Seal.DB.Misc.Common
       ( miscGetBi
       , miscPutBi
       ) where

import           Universum

import           Seal.Binary.Class (Bi)
import           Seal.DB.Class (DBTag (..), MonadDB, MonadDBRead)
import           Seal.DB.Functions (dbGetBi, dbPutBi)

miscGetBi
    :: forall v m . (MonadDBRead m, Bi v)
    => ByteString -> m (Maybe v)
miscGetBi = dbGetBi MiscDB

miscPutBi
    :: forall v m . (MonadDB m, Bi v)
    => ByteString -> v -> m ()
miscPutBi = dbPutBi MiscDB
