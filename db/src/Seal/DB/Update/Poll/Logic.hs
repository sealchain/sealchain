-- | Functions which operate on MonadPoll[Read]. Business logic of
-- Update System.

module Seal.DB.Update.Poll.Logic
       ( verifyAndApplyUSPayload
       , rollbackUS
       , normalizePoll
       , refreshPoll
       , filterProposalsByThd

       -- * Base
       , canCreateBlockBV

       -- * Softfork resolution
       , processGenesisBlock
       , recordBlockIssuance
       ) where

import           Seal.DB.Update.Poll.Logic.Apply
import           Seal.DB.Update.Poll.Logic.Base
import           Seal.DB.Update.Poll.Logic.Normalize
import           Seal.DB.Update.Poll.Logic.Rollback
import           Seal.DB.Update.Poll.Logic.Softfork
