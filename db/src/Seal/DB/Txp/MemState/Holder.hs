{-# LANGUAGE TypeFamilies #-}

-- | Monad transformer which implements MonadTxpMem based on ReaderT.

module Seal.DB.Txp.MemState.Holder
       ( GenericTxpLocalData
       , mkTxpLocalData
       ) where

import           Universum

import           Data.Default (Default (def))

import           Seal.Contract.Persist.MPTree (initMPtree)
import           Seal.DB.Class (MonadDBRead)
import           Seal.DB.GState.Common (getTip)
import           Seal.DB.Rocks (MonadRealDB)
import           Seal.DB.Txp.MemState.Types (GenericTxpLocalData (..))
import           Seal.DB.Txp.Pact (getPactStateRoot)
import           Seal.Mpt.MerklePatricia (getMPDB)

----------------------------------------------------------------------------
-- Holder
----------------------------------------------------------------------------

mkTxpLocalData
    :: (Default e, MonadIO m, MonadDBRead m, MonadRealDB ctx m)
    => m (GenericTxpLocalData e)
mkTxpLocalData = do
    initTip <- getTip
    pactStateRoot <- getPactStateRoot initTip
    pactMPDB <- getMPDB pactStateRoot
    TxpLocalData <$> 
        newTVarIO mempty <*> 
        newTVarIO def <*> 
        newTVarIO mempty <*> 
        newTVarIO mempty  <*>
        newTVarIO (initMPtree pactMPDB) <*>
        newTVarIO initTip <*>
        newTVarIO def
