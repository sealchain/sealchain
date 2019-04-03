{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Part of GState DB which stores stakes.

module Seal.DB.Txp.Stakes
       (
         -- * Operations
         StakesOp (..)

         -- * Initialization
       , initGStateStakes

         -- * Iteration
       , StakeIter
       , stakeSource
       , getAllPotentiallyHugeStakesMap

         -- * Sanity checks
       , sanityCheckStakes
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (Color (Red), colorize)
import           UnliftIO (MonadUnliftIO)

import           Seal.Binary.Class (serialize')
import           Seal.Chain.Genesis (GenesisData (..))
import           Seal.Chain.Txp (genesisStakes)
import           Seal.Core (Currency (..), Coin, StakeholderId, StakesMap)
import           Seal.Crypto (shortHashF)
import           Seal.DB (DBError (..), DBTag (GStateDB), IterType, MonadDB,
                     MonadDBRead, RocksBatchOp (..), dbIterSource)
import           Seal.DB.GState.Common (gsPutBi)
import           Seal.DB.GState.Stakes (StakeIter, ftsStakeKey, ftsSumKey,
                     getRealTotalStake)
import           Seal.Util.Wlog (WithLogger, logError)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

data StakesOp
    = PutTotalStake !Coin
    | PutFtsStake !StakeholderId
                  !Coin

instance Buildable StakesOp where
    build (PutTotalStake c) = bprint ("PutTotalStake ("%formatter%")") c
    build (PutFtsStake ad c) =
        bprint ("PutFtsStake ("%shortHashF%", "%formatter%")") ad c

instance RocksBatchOp StakesOp where
    toBatchOp (PutTotalStake c)  = [Rocks.Put ftsSumKey (serialize' c)]
    toBatchOp (PutFtsStake ad c) =
        if c == mkMoney 0 then [Rocks.Del (ftsStakeKey ad)]
        else [Rocks.Put (ftsStakeKey ad) (serialize' c)]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initGStateStakes :: forall m.(MonadDB m) => GenesisData -> m ()
initGStateStakes genesisData = do
    putFtsStakes
    putGenesisTotalStake
  where
    putTotalFtsStake :: Coin -> m ()
    putTotalFtsStake = gsPutBi ftsSumKey
    stakes = genesisStakes genesisData
    totalCoins = sumMoneys stakes
    -- Will 'error' if the result doesn't fit into 'Coin' (which should never
    -- happen)
    putGenesisTotalStake = putTotalFtsStake (unsafeIntegerToMoney totalCoins)
    putFtsStakes = mapM_ (uncurry putFtsStake) $ HM.toList stakes

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Run iterator over stakes.
stakeSource ::
       forall m. (MonadDBRead m)
    => ConduitT () (IterType StakeIter) (ResourceT m) ()
stakeSource = dbIterSource GStateDB (Proxy @StakeIter)

-- | Get stakes of all stakeholders. Use with care – the resulting map
-- can be very big.
getAllPotentiallyHugeStakesMap ::
       (MonadDBRead m, MonadUnliftIO m) => m StakesMap
getAllPotentiallyHugeStakesMap =
    runConduitRes $
    stakeSource .|
    CL.fold (\stakes (k, v) -> stakes & at k .~ Just v) mempty

----------------------------------------------------------------------------
-- Sanity checks
----------------------------------------------------------------------------

sanityCheckStakes
    :: (MonadDBRead m, MonadUnliftIO m, WithLogger m)
    => m ()
sanityCheckStakes = do
    calculatedTotalStake <- runConduitRes $
        mapOutput snd stakeSource .|
        CL.fold unsafeAddMoney (mkMoney 0)

    totalStake <- getRealTotalStake
    let fmt = ("Wrong real total stake: \
              \sum of real stakes: "%formatter%
              ", but getRealTotalStake returned: "%formatter)
    let msg = sformat fmt calculatedTotalStake totalStake
    unless (calculatedTotalStake == totalStake) $ do
        logError $ colorize Red msg
        throwM $ DBMalformed msg

----------------------------------------------------------------------------
-- Details
----------------------------------------------------------------------------

putFtsStake :: MonadDB m => StakeholderId -> Coin -> m ()
putFtsStake = gsPutBi . ftsStakeKey
