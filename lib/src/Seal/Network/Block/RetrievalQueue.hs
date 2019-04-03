-- | Block retrieval queue with accompanying datatypes.
module Seal.Network.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Universum

import           Control.Concurrent.STM (TBQueue)

import           Seal.Chain.Block (BlockHeader)
import           Seal.Infra.Network.Types (NodeId)

-- | Task that is put in the block retrieval queue for the retrieval
-- worker to perform.
data BlockRetrievalTask = BlockRetrievalTask
    { brtHeader    :: !BlockHeader
      -- ^ Header we're insterested in.
    , brtContinues :: !Bool
      -- ^ If it was tentatively classified as "direct continuation of
      -- our chain".
    }

data BlockRetrievalQueueTag

-- | Queue types.
type BlockRetrievalQueue = TBQueue (NodeId, BlockRetrievalTask)
