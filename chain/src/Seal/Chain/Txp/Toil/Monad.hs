{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

-- | Some monads used in Toil and primitive actions.

module Seal.Chain.Txp.Toil.Monad
       ( VerifyAndApplyM
       , VerifyAndApplyEnv (..)
       , vaaeUtxo
       , vaaeAcctMPDB
       , vaaePactMPDB
       , VerifyAndApplyState (..)
       , vaasUtxoModifier
       , vaasAcctModifier
       , vaasPactModifier

       , utxoGet
       , utxoPut
       , utxoDel

         -- * Monadic local Toil
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , ltsAcctModifier
       , ltsPactModifier
       , LocalToilEnv (..)
       , lteUtxo
       , lteAcctMPDB
       , ltePactMPDB
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo
       , ExtendedLocalToilM
       , extendLocalToilM

         -- * Monadic global Toil
       , GlobalToilState (..)
       , gtsUtxoModifier
       , gtsStakesView
       , gtsAcctModifier
       , gtsPactModifier
       , defGlobalToilState
       , GlobalToilEnv (..)
       , gteUtxo
       , gteAcctMPDB
       , gtePactMPDB
       , GlobalToilM
       , runGlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake
       , ExtendedGlobalToilM
       , extendGlobalToilM

         -- * Conversions
       , verifyAndApplyMToLocalToilM
       , verifyAndApplyMToGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (def)
import           Fmt ((+|), (|+))

import           Seal.Contract.Persist.MPTree (initMPtree)
import           Seal.Chain.Contract (PactModifier)
import           Seal.Chain.Txp.Toil.Types (MemPool, StakesView, UndoMap,
                     UtxoLookup, UtxoModifier, AccountModifier,
                     mpLocalTxs, mpSize, svStakes, svTotal)
import           Seal.Chain.Txp.Tx (TxId, TxIn)
import           Seal.Chain.Txp.TxAux (TxAux)
import           Seal.Chain.Txp.TxOutAux (TxOutAux)
import           Seal.Chain.Txp.Undo (TxUndo)
import           Seal.Core.Common (Coin, StakeholderId)
import           Seal.Mpt.MerklePatricia (MPDB(..))
import           Seal.Util (type (~>))
import qualified Seal.Util.Modifier as MM
import           Seal.Util.Wlog (NamedPureLogger, WithLogger, launchNamedPureLog)

----------------------------------------------------------------------------
-- Monad used for verify and apply.
----------------------------------------------------------------------------

data VerifyAndApplyState = VerifyAndApplyState
    { _vaasUtxoModifier :: !UtxoModifier
    , _vaasAcctModifier :: !AccountModifier
    , _vaasPactModifier :: !PactModifier
    }

makeLenses ''VerifyAndApplyState

data VerifyAndApplyEnv = VerifyAndApplyEnv
    { _vaaeUtxo     :: !UtxoLookup
    , _vaaeAcctMPDB :: !MPDB
    , _vaaePactMPDB :: !MPDB
    }

makeLenses ''VerifyAndApplyEnv

type VerifyAndApplyM m
     = ReaderT VerifyAndApplyEnv (StateT VerifyAndApplyState m)

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: (Monad m) => TxIn -> (VerifyAndApplyM m) (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- view vaaeUtxo
    MM.lookup utxoLookup txIn <$> use vaasUtxoModifier

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: (Monad m) => TxIn -> TxOutAux -> (VerifyAndApplyM m) ()
utxoPut txId txOut = utxoGet txId >>= \case
    Nothing -> vaasUtxoModifier %= MM.insert txId txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|txId|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: (Monad m) => TxIn -> (VerifyAndApplyM m) ()
utxoDel txId = utxoGet txId >>= \case
    Just _  -> vaasUtxoModifier %= MM.delete txId
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|txId|+" is not in the utxo")


----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

-- | Mutable state used in local Toil.
data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    , _ltsAcctModifier :: !AccountModifier
    , _ltsPactModifier :: !PactModifier
    }

makeLenses ''LocalToilState

data LocalToilEnv = LocalToilEnv
    { _lteUtxo       :: !UtxoLookup
    , _lteAcctMPDB   :: !MPDB
    , _ltePactMPDB   :: !MPDB
    }

makeLenses ''LocalToilEnv

-- | Monad in which local Toil happens.
type LocalToilM m = ReaderT LocalToilEnv (StateT LocalToilState m)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: Monad m => TxId -> LocalToilM m Bool
hasTx txId = isJust <$> use (ltsMemPool . mpLocalTxs . at txId)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: Monad m => TxId -> TxAux -> TxUndo -> LocalToilM m ()
putTxWithUndo txId tx undo =
    unlessM (hasTx txId) $ do
        ltsMemPool . mpLocalTxs . at txId .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at txId .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: Monad m => LocalToilM m Int
memPoolSize = use $ ltsMemPool . mpSize

-- | Extended version of 'LocalToilM'. It allows to put extra data
-- into reader context, extra state and also adds logging
-- capabilities. It's needed for explorer which has more complicated
-- transaction processing.
type ExtendedLocalToilM extraEnv extraState m =
    ReaderT (LocalToilEnv, extraEnv) (
        StateT (LocalToilState, extraState) (
            NamedPureLogger m
    ))

-- | Natural transformation from 'LocalToilM to 'ExtendedLocalToilM'.
extendLocalToilM :: Monad m => LocalToilM m a -> ExtendedLocalToilM extraEnv extraState m a
extendLocalToilM = mapReaderT (mapStateT lift . zoom _1) . magnify _1

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

-- | Mutable state used in global Toil.
data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    , _gtsAcctModifier :: !AccountModifier
    , _gtsPactModifier :: !PactModifier
    }

-- | Default 'GlobalToilState'.
defGlobalToilState :: MPDB -> GlobalToilState
defGlobalToilState pactMPDB =
    GlobalToilState 
    { _gtsUtxoModifier = mempty
    , _gtsStakesView = def
    , _gtsAcctModifier = mempty
    , _gtsPactModifier = initMPtree pactMPDB
    }

makeLenses ''GlobalToilState

-- | Immutable environment used in global Toil.
data GlobalToilEnv m = GlobalToilEnv
    { _gteUtxo        :: !UtxoLookup
    , _gteTotalStake  :: !Coin
    , _gteStakeGetter :: (StakeholderId -> m (Maybe Coin)) 
    , _gteAcctMPDB    :: !MPDB
    , _gtePactMPDB    :: !MPDB
    }

makeLenses ''GlobalToilEnv

-- | Monad in which global Toil happens.
type GlobalToilM m
     = ReaderT (GlobalToilEnv m) (StateT GlobalToilState (NamedPureLogger m))

runGlobalToilM 
    :: forall m a. (WithLogger m)
    => GlobalToilEnv m
    -> GlobalToilState
    -> GlobalToilM m a
    -> m (a, GlobalToilState)
runGlobalToilM env gts =
    launchNamedPureLog id . usingStateT gts . usingReaderT env

-- | Get stake of a given stakeholder.
getStake :: Monad m => StakeholderId -> GlobalToilM m (Maybe Coin)
getStake shId = do
    stakeGetter <- view gteStakeGetter
    (<|>) <$> use (gtsStakesView . svStakes . at shId) <*> (lift . lift . lift $ (stakeGetter shId))

-- | Get total stake of all stakeholders.
getTotalStake :: Monad m => GlobalToilM m Coin
getTotalStake =
    maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

-- | Set stake of a given stakeholder.
setStake :: Monad m => StakeholderId -> Coin -> GlobalToilM m ()
setStake shId c = gtsStakesView . svStakes . at shId .= Just c

-- | Set total stake of all stakeholders.
setTotalStake :: Monad m => Coin -> GlobalToilM m ()
setTotalStake c = gtsStakesView . svTotal .= Just c

-- | Extended version of 'GlobalToilM'. It allows to put extra data
-- into reader context and extra state. It's needed for explorer which
-- has more complicated transaction processing.
type ExtendedGlobalToilM extraEnv extraState m =
    ReaderT (GlobalToilEnv m, extraEnv) (
        StateT (GlobalToilState, extraState) (
            NamedPureLogger m
    ))

-- | Natural transformation from 'GlobalToilM to 'ExtendedGlobalToilM'.
extendGlobalToilM :: Monad m => GlobalToilM m ~> ExtendedGlobalToilM extraEnv extraState m
extendGlobalToilM = zoom _1 . magnify _1

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

verifyAndApplyMToLocalToilM :: forall a m.Monad m => VerifyAndApplyM m a -> LocalToilM m a
verifyAndApplyMToLocalToilM action = do
    utxoModifier <- use ltsUtxoModifier
    acctModifier <- use ltsAcctModifier
    pactModifier <- use ltsPactModifier
    utxoLookup <- view lteUtxo
    acctMPDB <- view lteAcctMPDB
    pactMPDB <- view ltePactMPDB

    (res, vaas) <- 
        lift . lift $
        usingStateT (VerifyAndApplyState utxoModifier acctModifier pactModifier) $
        usingReaderT (VerifyAndApplyEnv utxoLookup acctMPDB pactMPDB) $
        action

    ltsUtxoModifier .= _vaasUtxoModifier vaas
    ltsAcctModifier .= _vaasAcctModifier vaas
    ltsPactModifier .= _vaasPactModifier vaas
    return res

verifyAndApplyMToGlobalToilM :: forall a m.Monad m => VerifyAndApplyM m a -> GlobalToilM m a
verifyAndApplyMToGlobalToilM action = do
    utxoModifier <- use gtsUtxoModifier
    acctModifier <- use gtsAcctModifier
    pactModifier <- use gtsPactModifier
    utxoLookup <- view gteUtxo
    acctMPDB <- view gteAcctMPDB
    pactMPDB <- view gtePactMPDB

    (res, vaas) <- 
        lift . lift . lift $
        usingStateT (VerifyAndApplyState utxoModifier acctModifier pactModifier) $
        usingReaderT (VerifyAndApplyEnv utxoLookup acctMPDB pactMPDB) $
        action

    gtsUtxoModifier .= _vaasUtxoModifier vaas
    gtsAcctModifier .= _vaasAcctModifier vaas
    gtsPactModifier .= _vaasPactModifier vaas
    return res

