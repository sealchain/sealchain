{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for transaction processing (Txp)
-- and some useful getters and setters.

module Seal.DB.Txp.MemState.Class
       ( MonadTxpMem
       , TxpHolderTag
       , withTxpLocalData
       , withTxpLocalDataLog
       , getUtxoModifier
       , getLocalUndos
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getAccountModifier
       , getPactModifier
       , getTxpExtra
       , getTxpTip
       , setTxpLocalData
       , clearTxpMemPool

       , MonadTxpLocal (..)
       , TxpLocalWorkMode
       , MempoolExt
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Default (Default (..))
import qualified Data.HashMap.Strict as HM

import           Seal.Chain.Block (HeaderHash)
import           Seal.Chain.Contract (PactModifier)
import           Seal.Chain.Genesis as Genesis (Config)
import           Seal.Chain.Txp (MemPool (..), ToilVerFailure, TxAux, TxId,
                     TxpConfiguration, UndoMap, UtxoModifier, AccountModifier)
import           Seal.Core.Reporting (MonadReporting)
import           Seal.Core.Slotting (MonadSlots (..))
import           Seal.DB.Class (MonadDBRead, MonadGState (..))
import           Seal.DB.Rocks (MonadRealDB)
import           Seal.DB.Txp.MemState.Types (GenericTxpLocalData (..))
import           Seal.Util.Util (HasLens (..))
import           Seal.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext)
       , Default ext
       )

askTxpMem :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext)
askTxpMem = view (lensOf @TxpHolderTag)

-- | Operate with some or all of the TXP local data.
--
--   Since this function takes an STM action, it can be used to
--   read or modify the components.
withTxpLocalData
    :: (MonadIO m, MonadTxpMem e ctx m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
withTxpLocalData f = askTxpMem >>= \ld -> atomically (f ld)

-- | Operate with some of all of the TXP local data, allowing
--   logging.
withTxpLocalDataLog
    :: (MonadIO m, MonadTxpMem e ctx m, WithLogger m)
    => (GenericTxpLocalData e -> NamedPureLogger STM.STM a)
    -> m a
withTxpLocalDataLog f = askTxpMem >>=
    \ld -> launchNamedPureLog atomically $ f ld

-- | Read the UTXO modifier from the local TXP data.
getUtxoModifier
    :: GenericTxpLocalData e -> STM.STM UtxoModifier
getUtxoModifier = STM.readTVar . txpUtxoModifier

getLocalTxsMap
    :: GenericTxpLocalData e -> STM.STM (HashMap TxId TxAux)
getLocalTxsMap = fmap _mpLocalTxs . getMemPool

getLocalTxs
    :: GenericTxpLocalData e -> STM.STM [(TxId, TxAux)]
getLocalTxs = fmap HM.toList . getLocalTxsMap

getLocalUndos
    :: GenericTxpLocalData e -> STM.STM UndoMap
getLocalUndos = STM.readTVar . txpUndos

getAccountModifier
    :: GenericTxpLocalData e -> STM.STM AccountModifier
getAccountModifier = STM.readTVar . txpAccountModifier

getPactModifier
    :: GenericTxpLocalData e -> STM.STM PactModifier
getPactModifier = STM.readTVar . txpPactModifier

getMemPool
    :: GenericTxpLocalData e -> STM.STM MemPool
getMemPool = STM.readTVar . txpMemPool

getTxpTip
    :: GenericTxpLocalData e -> STM.STM HeaderHash
getTxpTip = STM.readTVar . txpTip

getTxpExtra
    :: GenericTxpLocalData e -> STM.STM e
getTxpExtra = STM.readTVar . txpExtra

-- | Helper function to set all components of the TxpLocalData.
setTxpLocalData
    :: GenericTxpLocalData e
    -> (UtxoModifier, MemPool, UndoMap, AccountModifier, PactModifier, HeaderHash, e)
    -> STM.STM ()
setTxpLocalData txpData (um, mp, un, am, pm, hh, e) = do
    STM.writeTVar (txpUtxoModifier txpData) um
    STM.writeTVar (txpMemPool txpData) mp
    STM.writeTVar (txpUndos txpData) un
    STM.writeTVar (txpAccountModifier txpData) am
    STM.writeTVar (txpPactModifier txpData) pm
    STM.writeTVar (txpTip txpData) hh
    STM.writeTVar (txpExtra txpData) e

-- | Clear everything in local data with the exception of the
--   header tip.
clearTxpMemPool
    :: Default e
    => PactModifier
    -> GenericTxpLocalData e
    -> STM ()
clearTxpMemPool pactModifier txpData = do
  tip <- getTxpTip txpData
  setTxpLocalData txpData (mempty, def, mempty, mempty, pactModifier, tip, def)

----------------------------------------------------------------------------
-- Abstract txNormalize and processTx
----------------------------------------------------------------------------

type family MempoolExt (m :: * -> *) :: *

class Monad m => MonadTxpLocal m where
    txpNormalize :: Genesis.Config -> TxpConfiguration -> m ()
    txpProcessTx :: Genesis.Config -> TxpConfiguration -> (TxId, TxAux) -> m (Either ToilVerFailure ())

type TxpLocalWorkMode ctx m =
    ( MonadIO m
    , MonadDBRead m
    , MonadGState m
    , MonadSlots ctx m
    , MonadTxpMem (MempoolExt m) ctx m
    , WithLogger m
    , MonadMask m
    , MonadReporting m
    , MonadRealDB ctx m
    )
