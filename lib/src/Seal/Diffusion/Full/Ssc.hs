{-# LANGUAGE RankNTypes #-}

module Seal.Diffusion.Full.Ssc
    ( sscListeners
    , sscOutSpecs
      -- TODO move the conversation starters here too (they're defined inline
      -- in Seal.Diffusion.Full).
    ) where

import           Universum

import           Data.Tagged (Tagged (..))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Message.Class (Message)

import           Seal.Binary.Class (Bi)
import           Seal.Binary.Limit (Limit)
-- Message instances for various types.
-- TODO should move these into the Diffusion module subtree.
import           Seal.Chain.Ssc (MCCommitment (..), MCOpening (..),
                     MCShares (..), MCVssCertificate (..))
import           Seal.Communication.Limits (mlMCCommitment, mlMCOpening,
                     mlMCShares, mlMCVssCertificate)
import           Seal.Core (StakeholderId)
import           Seal.Infra.Communication.Relay (DataMsg, InvOrData,
                     InvReqDataParams (..), MempoolParams (NoMempool),
                     Relay (..), ReqMsg, ReqOrRes, relayListeners,
                     relayPropagateOut)
import           Seal.Infra.Communication.Types.Protocol (EnqueueMsg,
                     MkListeners, MsgType (..), NodeId, OutSpecs)
import           Seal.Infra.Network.Types (Bucket)
import           Seal.Logic.Types (Logic (..))
import qualified Seal.Logic.Types as KV (KeyVal (..))
import           Seal.Util.Trace (Severity, Trace)

sscListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
sscListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (sscRelays logic)

sscRelays
    :: Logic IO
    -> [Relay]
sscRelays logic =
    [ commitmentRelay logic (postSscCommitment logic)
    , openingRelay (postSscOpening logic)
    , sharesRelay logic (postSscShares logic)
    , vssCertRelay (postSscVssCert logic)
    ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
sscOutSpecs :: Logic IO -> OutSpecs
sscOutSpecs logic = relayPropagateOut (sscRelays logic)

commitmentRelay
    :: Logic IO
    -> KV.KeyVal (Tagged MCCommitment StakeholderId) MCCommitment IO
    -> Relay
commitmentRelay logic kv = sscRelay kv (mlMCCommitment <$> getAdoptedBVData logic)

openingRelay
    :: KV.KeyVal (Tagged MCOpening StakeholderId) MCOpening IO
    -> Relay
openingRelay kv = sscRelay kv (pure mlMCOpening)

sharesRelay
    :: Logic IO
    -> KV.KeyVal (Tagged MCShares StakeholderId) MCShares IO
    -> Relay
sharesRelay logic kv = sscRelay kv (mlMCShares <$> getAdoptedBVData logic)

vssCertRelay
    :: KV.KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate IO
    -> Relay
vssCertRelay kv = sscRelay kv (pure mlMCVssCertificate)

sscRelay
    :: ( Buildable contents
       , Typeable contents
       , Bi (DataMsg contents)
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqOrRes (Tagged contents StakeholderId))
       , Message (ReqMsg (Tagged contents StakeholderId))
       )
    => KV.KeyVal (Tagged contents StakeholderId) contents IO
    -> IO (Limit contents)
    -> Relay
sscRelay kv mkLimit =
    InvReqData NoMempool $
        InvReqDataParams
          { invReqMsgType = MsgMPC
          , contentsToKey = KV.toKey kv
          , handleInv = \_ -> KV.handleInv kv
          , handleReq = \_ -> KV.handleReq kv
          , handleData = \_ -> KV.handleData kv
          , irdpMkLimit = mkLimit
          }
