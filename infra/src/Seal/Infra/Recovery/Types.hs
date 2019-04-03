module Seal.Infra.Recovery.Types
       ( RecoveryHeaderTag
       , RecoveryHeader
       ) where

import           Control.Concurrent.STM (TMVar)

import           Seal.Chain.Block (BlockHeader)
import           Seal.Infra.Communication.Types.Protocol (NodeId)

data RecoveryHeaderTag

type RecoveryHeader = TMVar (NodeId, BlockHeader)
