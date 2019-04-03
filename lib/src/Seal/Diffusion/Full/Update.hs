{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Seal.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal

       , updateListeners
       , updateOutSpecs
       ) where

import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ

import           Seal.Chain.Update (UpId, UpdateProposal, UpdateVote, mkVoteId)
import           Seal.Communication.Limits (mlUpdateProposalAndVotes,
                     mlUpdateVote)
import           Seal.Infra.Communication.Protocol (EnqueueMsg, MkListeners,
                     MsgType (..), NodeId, Origin (..), OutSpecs)
import           Seal.Infra.Communication.Relay (InvReqDataParams (..),
                     MempoolParams (..), Relay (..), invReqDataFlowTK,
                     relayListeners, relayPropagateOut)
import           Seal.Infra.Network.Types (Bucket)
import           Seal.Logic.Types (Logic (..))
import qualified Seal.Logic.Types as KV (KeyVal (..))
import           Seal.Util.Trace (Severity, Trace)

-- Send UpdateVote to given addresses.
sendVote
    :: Trace IO (Severity, Text)
    -> EnqueueMsg
    -> UpdateVote
    -> IO ()
sendVote logTrace enqueue vote =
    void $ invReqDataFlowTK
        logTrace
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId vote)
        vote

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: Trace IO (Severity, Text)
    -> EnqueueMsg
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> IO ()
sendUpdateProposal logTrace enqueue upid proposal votes = do
    void $ invReqDataFlowTK
        logTrace
        "UpdateProposal"
        enqueue
        (MsgMPC OriginSender)
        upid
        (proposal, votes)

updateListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
updateListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (usRelays logic)

-- | Relays for data related to update system
usRelays :: Logic IO -> [Relay]
usRelays logic = [proposalRelay logic, voteRelay logic]

-- | 'OutSpecs' for the update system, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
updateOutSpecs :: Logic IO -> OutSpecs
updateOutSpecs logic = relayPropagateOut (usRelays logic)

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay :: Logic IO -> Relay
proposalRelay logic =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = KV.toKey kv
           , handleInv = \_ -> KV.handleInv kv
           , handleReq = \_ -> KV.handleReq kv
           , handleData = \_ -> KV.handleData kv
           , irdpMkLimit = mlUpdateProposalAndVotes <$> getAdoptedBVData logic
           }
  where
    kv = postUpdate logic

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

voteRelay :: Logic IO -> Relay
voteRelay logic =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = KV.toKey kv
           , handleInv = \_ -> KV.handleInv kv
           , handleReq = \_ -> KV.handleReq kv
           , handleData = \_ -> KV.handleData kv
           , irdpMkLimit = pure mlUpdateVote
           }
  where
    kv = postVote logic
