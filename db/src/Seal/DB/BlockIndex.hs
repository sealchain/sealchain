-- | Operations with block index db.

module Seal.DB.BlockIndex
       ( getHeader
       , getTipHeader
       , putHeadersIndex
       , deleteHeaderIndex
       ) where

import           Universum

import           Data.ByteArray (convert)

import qualified Database.RocksDB as Rocks

import           Seal.Binary.Class (serialize')
import           Seal.Chain.Block (BlockHeader, HeaderHash, headerHash)
import           Seal.DB.Class (DBTag (BlockIndexDB), MonadBlockDBRead,
                     MonadDB (..))
import           Seal.DB.Functions (dbGetBi)
import           Seal.DB.GState.Common (getTipSomething)

-- | Returns header of block that was requested from Block DB.
getHeader
    :: (MonadBlockDBRead m)
    => HeaderHash -> m (Maybe BlockHeader)
getHeader = dbGetBi BlockIndexDB . blockIndexKey

-- | Get 'BlockHeader' corresponding to tip.
getTipHeader :: MonadBlockDBRead m => m BlockHeader
getTipHeader = getTipSomething "header" getHeader

-- | Writes batch of headers into the block index db.
putHeadersIndex :: (MonadDB m) => [BlockHeader] -> m ()
putHeadersIndex =
    dbWriteBatch BlockIndexDB .
    map (\h -> Rocks.Put (blockIndexKey $ headerHash h) (serialize' h))

-- | Deletes header from the index db.
deleteHeaderIndex :: MonadDB m => HeaderHash -> m ()
deleteHeaderIndex = dbDelete BlockIndexDB . blockIndexKey

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

blockIndexKey :: HeaderHash -> ByteString
blockIndexKey h = "b" <> convert h
