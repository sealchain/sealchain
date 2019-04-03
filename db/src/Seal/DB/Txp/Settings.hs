{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}

-- | Global settings of Txp.

module Seal.DB.Txp.Settings
       ( TxpCommonMode
       , TxpGlobalVerifyMode
       , TxpGlobalApplyMode
       , TxpGlobalRollbackMode
       , TxpBlock
       , TxpBlund
       , TxpGlobalSettings (..)
       ) where

import           Universum

import           Seal.Chain.Block (ComponentBlock)
import           Seal.Chain.Txp (ToilVerFailure, TxPayload, TxpUndo)
import           Seal.Core.Chrono (NE, NewestFirst, OldestFirst)
import           Seal.Core.Slotting (MonadSlots)
import           Seal.DB (MonadDBRead, MonadRealDB, MonadGState, SomeBatchOp)
import           Seal.Util.Wlog (WithLogger)

type TxpCommonMode ctx m =
    ( WithLogger m
    , MonadDBRead m
    , MonadGState m
    , MonadRealDB ctx m
    )

type TxpGlobalVerifyMode ctx m =
    ( TxpCommonMode ctx m
    , MonadRealDB ctx m
    )

type TxpGlobalApplyMode ctx m =
    ( TxpCommonMode ctx m
    , MonadSlots ctx m  -- TODO: I don't like it (@gromak)
    , MonadRealDB ctx m
    )

type TxpGlobalRollbackMode ctx m = TxpCommonMode ctx m
type TxpBlock = ComponentBlock TxPayload
type TxpBlund = (TxpBlock, TxpUndo)

data TxpGlobalSettings = TxpGlobalSettings
    { -- | Verify a chain of payloads from blocks and return txp undos
      -- for each payload.
      --
      -- First argument determines whether it should be checked that
      -- all data from transactions is known (script versions,
      -- attributes, addresses, witnesses).
      tgsVerifyBlocks :: forall ctx m. TxpGlobalVerifyMode ctx m =>
                         Bool -> OldestFirst NE TxpBlock ->
                         m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
    , -- | Apply chain of /definitely/ valid blocks to Txp's GState.
      tgsApplyBlocks :: forall ctx m . TxpGlobalApplyMode ctx m =>
                        OldestFirst NE TxpBlund -> m SomeBatchOp
    , -- | Rollback chain of blocks.
      tgsRollbackBlocks :: forall ctx m . TxpGlobalRollbackMode ctx m =>
                           NewestFirst NE TxpBlund -> m SomeBatchOp
    }
