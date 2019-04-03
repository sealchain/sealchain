{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types related to rocksdb implementation 'MonadDBRead' and
-- 'MonadDB'.
--
-- 'MonadRealDB' is the most featured class (actually just a set of
-- constraints) which wraps 'NodeDBs' containing RocksDB
-- databases. This class can be used to manipulate RocksDB
-- directly. It may be useful when you need an access to advanced
-- features of RocksDB.

module Seal.RocksDB.Types
       ( MonadRealDB
       , NodeDBs (..)
       , DB (..)

       , blockIndexDB
       , blockDataDir
       , epochDataDir
       , epochLock
       , gStateDB
       , lrcDB
       , miscDB
       , mpDB

       , getNodeDBs
       ) where

import           Universum

import           Control.Lens (makeLenses)
import qualified Database.RocksDB as Rocks

import           Seal.Util.Concurrent.RWLock (RWLock)
import           Seal.Util.Util (HasLens (..))


-- | This is the set of constraints necessary to operate on «real» DBs
-- (which are wrapped into 'NodeDBs').  Apart from providing access to
-- 'NodeDBs' it also has 'MonadIO' constraint, because it's impossible
-- to use real DB without IO. Finally, it has 'MonadCatch' constraints
-- (partially for historical reasons, partially for good ones).
type MonadRealDB ctx m =
    ( MonadReader ctx m
    , HasLens NodeDBs ctx NodeDBs
    , MonadIO m
    , MonadCatch m
    )

data NodeDBs = NodeDBs
    { _blockIndexDB :: !DB       -- ^ Block index.
    , _blockDataDir :: !FilePath -- ^ Block and undo files.
    , _epochDataDir :: !FilePath -- ^ Epoch files.
    , _gStateDB     :: !DB       -- ^ Global state corresponding to some tip.
    , _lrcDB        :: !DB       -- ^ Data computed by LRC.
    , _miscDB       :: !DB       -- ^ Everything small and insignificant
    , _epochLock    :: !RWLock   -- ^ Lock for the epoch file consolidation.
    , _mpDB         :: !DB
    }

data DB = DB
    { rocksReadOpts  :: !Rocks.ReadOptions
    , rocksWriteOpts :: !Rocks.WriteOptions
    , rocksOptions   :: !Rocks.Options
    , rocksDB        :: !Rocks.DB
    }

makeLenses ''NodeDBs

getNodeDBs :: MonadRealDB ctx m => m NodeDBs
getNodeDBs = view (lensOf @NodeDBs)
