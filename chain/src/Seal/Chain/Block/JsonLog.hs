{-# LANGUAGE RecordWildCards #-}

module Seal.Chain.Block.JsonLog
       ( jlCreatedBlock
       , jlAdoptedBlock
       ) where

import           Universum

import           Formatting (sformat)

import           Seal.Chain.Block.Block (Block, gbHeader, genBlockEpoch,
                     mainBlockSlot, mainBlockTxPayload)
import           Seal.Chain.Block.Header (HeaderHash, gbhPrevBlock, headerHash,
                     headerHashF)
import           Seal.Chain.Txp (txpTxs)
import           Seal.Core (SlotCount, SlotId (..), getEpochIndex, getSlotIndex,
                     mkLocalSlotIndex)
import           Seal.Core.JsonLog.LogEvents (JLBlock (..), JLEvent (..))
import           Seal.Crypto (hash, hashHexF)

-- | Return event of created block.
jlCreatedBlock :: SlotCount -> Block -> JLEvent
jlCreatedBlock epochSlots block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHeaderHash $ headerHash block
    jlPrevBlock = showHeaderHash $ case block of
        Left  gB -> view gbhPrevBlock (gB ^. gbHeader)
        Right mB -> view gbhPrevBlock (mB ^. gbHeader)
    jlSlot = (getEpochIndex $ siEpoch slot, getSlotIndex $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. mainBlockTxPayload . txpTxs
    slot :: SlotId
    slot = case block of
        Left  gB -> let slotZero = case mkLocalSlotIndex epochSlots 0 of
                                        Right sz -> sz
                                        Left _   -> error "impossible branch"
                    in SlotId (gB ^. genBlockEpoch) slotZero
        Right mB -> mB ^. mainBlockSlot
    fromTx = sformat hashHexF . hash

-- | Returns event of created 'Block'.
jlAdoptedBlock :: Block -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHeaderHash . headerHash

showHeaderHash :: HeaderHash -> Text
showHeaderHash = sformat headerHashF
