module Seal.Core.Common.Stakes
       ( StakesMap
       , StakesList
       ) where

import           Universum

import           Seal.Core.Common.Coin
import           Seal.Core.Common.StakeholderId

-- | A mapping between stakeholders and they stakes.
type StakesMap = HashMap StakeholderId Coin

-- | Stakeholders and their stakes.
type StakesList = [(StakeholderId, Coin)]
