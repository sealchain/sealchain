{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Part of mpt DB which stores pact state

module Seal.DB.Txp.Pact
       ( PactStateRootOp (..)
       , PactStateRootIter
       , getPactStateRoot
       , getTableStateRoot
       , getAllTableData
       , getDataInTable
       , getDiffDataInTable
       ) where


import           Universum

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BC                      
import qualified Database.RocksDB as Rocks

import           Seal.Binary.Class (serialize')
import           Seal.Chain.Block (HeaderHash)
import           Seal.DB (DBIteratorClass (..), MonadDBRead, RocksBatchOp (..),
                     encodeWithKeyPrefix)
import           Seal.DB.Rocks (MonadRealDB)   
import           Seal.DB.GState.Common (gsGetBi)
import           Seal.Mpt.MerklePatricia
import           Seal.Mpt.MerklePatricia.Utils(bytesToNibbleString,transKV)

data PactStateRootOp
    = PutPactStateRoot !HeaderHash !StateRoot

instance RocksBatchOp PactStateRootOp where
    toBatchOp (PutPactStateRoot hh sr) = [Rocks.Put (headerHashKey hh) (serialize' sr)]

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getPactStateRoot :: MonadDBRead m => HeaderHash -> m StateRoot
getPactStateRoot hh = fromMaybe emptyTriePtr <$> (gsGetBi $ headerHashKey hh)

getTableStateRoot :: MonadRealDB ctx m => StateRoot-> String -> m (Maybe StateRoot)
getTableStateRoot root tableName = do
    mpDB <- getMPDB root
    stateRootM <- getKeyVal mpDB $ bytesToNibbleString $ BC.fromString tableName
    case stateRootM of
        Just sr -> return $ Just $ StateRoot sr
        Nothing -> return Nothing
    
getAllTableData :: MonadRealDB ctx m => StateRoot -> m ([(String,String)])
getAllTableData root = do
    mpDB <- getMPDB root
    allTableDatas <- getAllKeyVals mpDB
    return $ map (decode . transKV) allTableDatas
    where
        decode :: (B.ByteString, B.ByteString) -> (String,String)
        decode kvs  =  (BC.toString $ fst kvs,BC.toString $ snd kvs)   


getDataInTable :: MonadRealDB ctx m => StateRoot -> m ([(String,String)])
getDataInTable root = do
    mpDB <- getMPDB root
    allTableDatas <- getAllKeyVals mpDB
    return $ map (decodedata . transKV) allTableDatas
    where
        decodedata :: (B.ByteString, B.ByteString) -> (String,String)
        decodedata kvs  =  (BC.toString $ fst kvs,BC.toString $ snd kvs)  
 
getDiffDataInTable :: MonadRealDB ctx m => StateRoot -> StateRoot -> m ([(String,String)])
getDiffDataInTable root preRoot= do
    mpDB <- getMPDB root
    preMpDB <- getMPDB preRoot
    diffTableDatas <- getDiffKeyVals mpDB preMpDB
    return $ map (decodedata . transKV) diffTableDatas
    where
        decodedata :: (B.ByteString, B.ByteString) -> (String,String)
        decodedata kvs  =  (BC.toString $ fst kvs,BC.toString $ snd kvs)        
----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

data PactStateRootIter

instance DBIteratorClass PactStateRootIter where
    type IterKey PactStateRootIter = HeaderHash
    type IterValue PactStateRootIter = StateRoot
    iterKeyPrefix = iterationStateRootPrefix

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

headerHashKey :: HeaderHash -> ByteString
headerHashKey = encodeWithKeyPrefix @PactStateRootIter

iterationStateRootPrefix :: ByteString
iterationStateRootPrefix = "st/p/"
