{-# LANGUAGE OverloadedStrings #-}

module Seal.Mpt.MerklePatricia.MPDB (
  MPDB(..),
  getMPDB,
  {-openMPDB-}
  ) where

import           Universum

import           Seal.RocksDB (DB (..), MonadRealDB, mpDB, getNodeDBs)
import           Seal.Mpt.MerklePatricia.StateRoot

-- | This is the database reference type, contianing both the handle to the underlying database, as well
-- as the stateRoot to the current tree holding the data.
--
-- The MPDB acts a bit like a traditional database handle, although because it contains the stateRoot,
-- many functions act by updating its value.  Because of this, it is recommended that this item be
-- stored and modified within the state monad.
data MPDB = MPDB {
    rdb       :: DB,
    stateRoot :: StateRoot
}

getMPDB :: MonadRealDB ctx m => StateRoot -> m MPDB
getMPDB root = do
    db <- view mpDB <$> getNodeDBs
    return $ MPDB db root

-- | This function is used to create an MPDB object corresponding to the blank database.
-- After creation, the stateRoot can be changed to a previously saved version.
{-openMPDB::(MonadIO m)
        =>FilePath->m MPDB
openMPDB path = do
  rdb' <- openRocksDB path

  Rocks.put rdb' Rocks.defaultWriteOptions (BL.toStrict $ encode emptyTriePtr) B.empty
  return MPDB{ rdb=rdb', stateRoot=emptyTriePtr }
-}
{--openMPDB::String -- ^ The filepath with the location of the underlying database.
        ->ResourceT IO MPDB
openMPDB path = do
  rdb' <- DB.open ((DB.defaultOptions path) {DB.optionsCreateIfMissing = True})
  DB.put rdb' DB.defaultWriteOptions (BL.toStrict $ encode emptyTriePtr) B.empty
  return MPDB{ rdb=rdb', stateRoot=emptyTriePtr }
--}
