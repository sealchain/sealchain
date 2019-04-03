-- | This is an implementation of the modified Merkle Patricia database
-- described in the Ethereum Yellowpaper
-- (<http://gavwood.com/paper.pdf>).  This modified version works like a
-- canonical Merkle Patricia database, but includes certain
-- optimizations.  In particular, a new type of "shortcut node" has been
-- added to represent multiple traditional nodes that fall in a linear
-- string (ie- a stretch of parent child nodes where no branch choices
-- exist).
--
-- A Merkle Patricia Database effeciently retains its full history, and a
-- snapshot of all key-value pairs at a given time can be looked up using
-- a "stateRoot" (a pointer to the root of the tree representing that
-- data).  Many of the functions in this module work by updating this
-- object, so for anything more complicated than a single update, use of
-- the state monad is recommended.
--
-- The underlying data is actually stored in LevelDB.  This module
-- provides the logic to organize the key-value pairs in the appropriate
-- Patricia Merkle Tree.

module Seal.Mpt.MerklePatricia 
 ( MPKey
 , MPVal
 , MPDB (..)
 , getMPDB

 , StateRoot (..)
 , emptyTriePtr
 , sha2StateRoot
 , unboxStateRoot

 , putKeyVal
 , getKeyVal
 , deleteKey
 , keyExists
 , getAllKeyVals
 , getDiffKeyVals
 , initializeBlank
 , assertStateRootSuc
 , getKeyValM
 , putKeyValM
 , deleteKeyM
 , commitAccountModifier
 ) where

import           Universum hiding (head)

import           Data.List hiding (null)
import           Data.Binary (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Database.RocksDB as DB

import           Seal.Binary.Class (serialize')
import           Seal.Core (AccountModifier)
import           Seal.RocksDB (DB (..))
import           Seal.Mpt.MerklePatricia.Internal
import           Seal.Mpt.MerklePatricia.MPDB
import           Seal.Mpt.MerklePatricia.StateRoot
import           Seal.Mpt.MerklePatricia.Utils
import qualified Seal.Util.Modifier as MM

-- | Adds a new key/value pair.
putKeyVal::(MonadIO m) => MPDB -- ^ The object containing the current stateRoot.
           -> MPKey -- ^ MPKey of the data to be inserted.
           -> MPVal -- ^ Value of the new data
           -> m MPDB -- ^ The object containing the stateRoot to the data after the insert.
putKeyVal db = unsafePutKeyVal db

-- | Retrieves all key/value pairs whose key starts with the given parameter.
getKeyVal::(MonadIO m) => MPDB -- ^ Object containing the current stateRoot.
         -> MPKey -- ^ MPKey of the data to be inserted.
         -> m (Maybe MPVal) -- ^ The requested value.
getKeyVal db key = do
    vals <- unsafeGetKeyVals db key
    let kvs' = filter keyFilter vals
    return $
      if not (null kvs')
      then Just $ snd $ head kvs'
      else Nothing
    where
        keyFilter kvs = (fst kvs) == key
    
        
getAllKeyVals::(MonadIO m) => MPDB -> m [(MPKey, MPVal)]
getAllKeyVals = unsafeGetAllKeyVals

getDiffKeyVals::(MonadIO m)=> MPDB -> MPDB -> m [(MPKey, MPVal)]
getDiffKeyVals db preDb = do
  kvs <- getAllKeyVals db
  preKvs <- getAllKeyVals preDb

  let curSet = Set.fromList kvs
  let preSet = Set.fromList preKvs

  let rsSet = Set.intersection curSet (Set.difference curSet preSet)
  return $ Set.toList rsSet
      
-- | Deletes a key (and its corresponding data) from the database.
--
-- Note that the key/value pair will still be present in the history, and
-- can be accessed by using an older 'MPDB' object.
deleteKey :: (MonadIO m) => MPDB -- ^ The object containing the current stateRoot.
          -> MPKey -- ^ The key to be deleted.
          -> m MPDB -- ^ The object containing the stateRoot to the data after the delete.
deleteKey db = unsafeDeleteKey db

-- | Returns True is a key exists.
keyExists :: (MonadIO m) => MPDB -- ^ The object containing the current stateRoot.
          -> MPKey -- ^ The key to be deleted.
          -> m Bool -- ^ True if the key exists
keyExists db key = isJust <$> getKeyVal db key

-- | Initialize the DB by adding a blank stateroot.
initializeBlank :: MonadIO m => MPDB -- ^ The object containing the current stateRoot.
                -> m ()
initializeBlank db =
    {-let bytes = serialize' (0::Integer)
        StateRoot key = emptyTriePtr
    in DB.put (rocksDB $ rdb db) DB.defaultWriteOptions  key bytes-}
    DB.put (rocksDB $ rdb db) DB.defaultWriteOptions  (BL.toStrict $ encode emptyTriePtr) B.empty

putKeyValM :: MonadIO m => MPKey -> MPVal -> StateT MPDB m ()
putKeyValM k v = StateT $ \db -> ((), ) <$> putKeyVal db k v

getKeyValM :: MonadIO m => MPKey -> StateT MPDB m (Maybe MPVal)
getKeyValM k = get >>= \db -> getKeyVal db k

deleteKeyM :: MonadIO m => MPKey -> StateT MPDB m ()
deleteKeyM k = StateT $ \db -> ((), ) <$> deleteKey db k

commitAccountModifier :: MonadIO m => AccountModifier -> MPDB -> m MPDB
commitAccountModifier mm mpdb = do
    (_, mpdb') <- runStateT go mpdb
    return mpdb'
  where 
    acctToKey = bytesToNibbleString . serialize'
    go = do
        forM_ (HM.toList (MM.toHashMap mm)) $
            \(k, mv) -> case mv of
                Nothing -> deleteKeyM (acctToKey k)
                Just v -> putKeyValM (acctToKey k) (serialize' v)
