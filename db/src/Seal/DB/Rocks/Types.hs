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

module Seal.DB.Rocks.Types
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

       , getDBByTag
       , getBlockIndexDB
       , getGStateDB
       , getLrcDB
       , getMiscDB
       ) where

import           Universum

import           Seal.RocksDB (DB (..), NodeDBs (..), MonadRealDB, 
                     blockIndexDB, blockDataDir, epochDataDir, epochLock, 
                     gStateDB, lrcDB, miscDB, mpDB, getNodeDBs)
import           Seal.DB.Class (DBTag (..))

dbTagToLens :: DBTag -> Lens' NodeDBs DB
dbTagToLens BlockIndexDB = blockIndexDB
dbTagToLens GStateDB     = gStateDB
dbTagToLens LrcDB        = lrcDB
dbTagToLens MiscDB       = miscDB

getDBByTag :: MonadRealDB ctx m => DBTag -> m DB
getDBByTag tag = view (dbTagToLens tag) <$> getNodeDBs

getBlockIndexDB :: MonadRealDB ctx m => m DB
getBlockIndexDB = getDBByTag BlockIndexDB

getGStateDB :: MonadRealDB ctx m => m DB
getGStateDB = getDBByTag GStateDB

getLrcDB :: MonadRealDB ctx m => m DB
getLrcDB = getDBByTag LrcDB

getMiscDB :: MonadRealDB ctx m => m DB
getMiscDB = getDBByTag MiscDB