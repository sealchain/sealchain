{-# LANGUAGE RecordWildCards #-}

module Seal.Chain.Security.Util
       ( shouldIgnoreAddress
       , shouldIgnorePkAddress
       ) where

import           Universum


import           Seal.Chain.Security.Params (AttackTarget (..), AttackType (..),
                     SecurityParams (..))
import           Seal.Core (StakeholderId)
import           Seal.Core.NetworkAddress (NetworkAddress)
import           Seal.Util.Util (HasLens (..))

shouldIgnoreAddress
    :: (MonadReader ctx m, HasLens SecurityParams ctx SecurityParams)
    => NetworkAddress -> m Bool
shouldIgnoreAddress addr = do
    SecurityParams{..} <- view (lensOf @SecurityParams)
    return $ and [
        AttackNoBlocks `elem` spAttackTypes,
        NetworkAddressTarget addr `elem` spAttackTargets ]

shouldIgnorePkAddress
    :: (MonadReader ctx m, HasLens SecurityParams ctx SecurityParams)
    => StakeholderId -> m Bool
shouldIgnorePkAddress addr = do
    SecurityParams{..} <- view (lensOf @SecurityParams)
    return $ and [
        AttackNoCommitments `elem` spAttackTypes,
        PubKeyAddressTarget addr `elem` spAttackTargets ]
