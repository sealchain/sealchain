{-# LANGUAGE TypeFamilies #-}

-- | Instance of MoandPollRead which uses DB.

module Seal.DB.Update.Poll.DBPoll
       ( DBPoll
       , runDBPoll
       ) where

import           Universum hiding (id)

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Ether
import           UnliftIO (MonadUnliftIO)

import           Seal.Chain.Lrc (FullRichmenData)
import           Seal.Chain.Update (HasUpdateConfiguration, MonadPollRead (..))
import           Seal.Core (Coin)
import           Seal.DB.Class (MonadDBRead)
import           Seal.DB.Lrc (HasLrcContext, getIssuersStakes,
                     lrcActionOnEpochReason, tryGetUSRichmen)
import qualified Seal.DB.Update.GState as GS
import           Seal.Util.Wlog (WithLogger)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

data DBPollTag

type DBPoll = Ether.TaggedTrans DBPollTag IdentityT

runDBPoll :: DBPoll m a -> m a
runDBPoll = coerce

instance ( MonadIO m
         , MonadDBRead m
         , MonadUnliftIO m
         , WithLogger m
         , MonadReader ctx m
         , HasLrcContext ctx
         , HasUpdateConfiguration
         ) =>
         MonadPollRead (DBPoll m) where
    getBVState = GS.getBVState
    getProposedBVs = GS.getProposedBVs
    getEpochProposers = GS.getEpochProposers
    getCompetingBVStates = GS.getCompetingBVStates
    getAdoptedBVFull = GS.getAdoptedBVFull
    getLastConfirmedSV = GS.getConfirmedSV
    getProposal = GS.getProposalState
    getProposalsByApp = GS.getProposalsByApp
    getConfirmedProposals = GS.getConfirmedProposals Nothing
    getEpochTotalStake genesisBvd e = fmap fst <$> tryGetUSRichmen genesisBvd e
    getRichmanStake genesisBvd e id =
        (findStake =<<) <$> tryGetUSRichmen genesisBvd e
      where
        findStake :: FullRichmenData -> Maybe Coin
        findStake = HM.lookup id . snd
    getOldProposals = GS.getOldProposals
    getDeepProposals = GS.getDeepProposals
    getBlockIssuerStake epoch id =
        lrcActionOnEpochReason epoch
            "couldn't get issuers's stakes"
            (fmap (Just . HM.lookup id) . getIssuersStakes)
    getSlottingData = GS.getSlottingData
