module Seal.Chain.Ssc.OpeningsMap
       ( OpeningsMap
       ) where

import           Universum

import           Seal.Core.Common (StakeholderId)

import           Seal.Chain.Ssc.Opening

type OpeningsMap = HashMap StakeholderId Opening
