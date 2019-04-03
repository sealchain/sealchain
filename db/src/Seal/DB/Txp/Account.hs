{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of mpt DB which stores account state

module Seal.DB.Txp.Account
       ( AccountStateRootOp (..)
       , AccountStateRootIter
       , putAccountState
       , getAccountState
       , getAccountStateRoot
       , getAllAccountState
       , getDiffAccountState
       ) where


import           Universum
import qualified Universum.Unsafe as Unsafe

import qualified Data.ByteString as B
import qualified Database.RocksDB as Rocks
import qualified Formatting.Buildable
import           Formatting (bprint, build, (%))
import qualified Serokell.Util.Base16 as B16

import           Seal.Binary.Class (serialize', decodeFull')
import           Seal.Chain.Block (HeaderHash)
import           Seal.Core.Common (Account,AccountState)
import           Seal.DB (DBIteratorClass (..), MonadDBRead, RocksBatchOp (..), 
                     encodeWithKeyPrefix)
import           Seal.DB.GState.Common (gsGetBi)                         
import           Seal.DB.Rocks (MonadRealDB)
import           Seal.Mpt.MerklePatricia
import           Seal.Mpt.MerklePatricia.Utils (bytesToNibbleString, transKV)

data AccountStateRootOp
    = PutAccountStateRoot !HeaderHash !StateRoot

instance Buildable AccountStateRootOp where
    build (PutAccountStateRoot headerHash (StateRoot sr)) =
        bprint ("PutAccountStateRoot ("%build%", "%build%")")
        headerHash (B16.encode sr)

instance RocksBatchOp AccountStateRootOp where
    toBatchOp (PutAccountStateRoot hh sr) = [Rocks.Put (headerHashKey hh) (serialize' sr)]


----------------------------------------------------------------------------
-- Put account state to mpt
-- Not sure is OK?
---------------------------------------------------------------------------- 
putAccountState :: MonadRealDB ctx m => StateRoot -> Account -> AccountState -> m StateRoot
putAccountState root account accountState = do
    mpDB <- getMPDB root
    newMPDB <- putKeyVal mpDB (bytesToNibbleString $ serialize' account) (serialize' accountState)
    return $ stateRoot newMPDB

getAccountState :: MonadRealDB ctx m => StateRoot -> Account -> m (Maybe AccountState)
getAccountState root account = do
    mpDB <- getMPDB root
    mbAccountState <- getKeyVal mpDB $ bytesToNibbleString $ serialize' account
    case mbAccountState of
         Just accountState -> 
             case decodeFull' accountState of
                  Right accountState' -> return $ Just accountState'   
                  Left _ -> return Nothing
         Nothing -> return Nothing
 
getAllAccountState :: MonadRealDB ctx m => StateRoot -> m ([(Account,AccountState)])
getAllAccountState root = do
    mpDB <- getMPDB root
    accountStates <- getAllKeyVals mpDB  -- bytesToNibbleString $ serialize' account  [(MPKey, MPVal)]
    return $ map (decode . transKV) accountStates
    where
        decode :: (B.ByteString, B.ByteString) -> (Account, AccountState)
        decode aas  =  (Unsafe.fromJust $ getKey $ fst aas,Unsafe.fromJust $ getValue $ snd aas)

        getKey key = case decodeFull' key of
                        Right key' ->  Just key' 
                        Left _ -> Nothing
        getValue value = case decodeFull' value of
                            Right value' -> Just value' 
                            Left _ -> Nothing
                            
getDiffAccountState :: MonadRealDB ctx m => StateRoot -> StateRoot -> m ([(Account, AccountState)])
getDiffAccountState root preRoot = do
    mpDB <- getMPDB root
    preMpDb <- getMPDB preRoot
    diffAccountState <- getDiffKeyVals mpDB preMpDb
    return $ map (decode . transKV) diffAccountState
    where
        decode :: (B.ByteString, B.ByteString) -> (Account, AccountState)
        decode aas  =  (Unsafe.fromJust $ getKey $ fst aas,Unsafe.fromJust $ getValue $ snd aas)

        getKey key = case decodeFull' key of
                        Right key' ->  Just key' 
                        Left _ -> Nothing
        getValue value = case decodeFull' value of
                            Right value' -> Just value' 
                            Left _ -> Nothing                            
----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getAccountStateRoot :: MonadDBRead m => HeaderHash -> m StateRoot
getAccountStateRoot hh = fromMaybe emptyTriePtr <$> (gsGetBi $ headerHashKey hh)

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data AccountStateRootIter

instance DBIteratorClass AccountStateRootIter where
    type IterKey AccountStateRootIter = HeaderHash
    type IterValue AccountStateRootIter = StateRoot
    iterKeyPrefix = iterationStateRootPrefix

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

headerHashKey :: HeaderHash -> ByteString
headerHashKey = encodeWithKeyPrefix @AccountStateRootIter

iterationStateRootPrefix :: ByteString
iterationStateRootPrefix = "st/a/"
