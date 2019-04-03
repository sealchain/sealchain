{-# LANGUAGE RankNTypes #-}

-- | Send and receive for delegation.

module Seal.Diffusion.Full.Delegation
       ( delegationListeners
       , delegationOutSpecs
       , sendPskHeavy
       ) where

import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ

import           Seal.Binary ()
import           Seal.Chain.Delegation (ProxySKHeavy)
import           Seal.Communication.Limits (mlHeavyDlgIndex, mlProxySecretKey)
import           Seal.Infra.Communication.Protocol (EnqueueMsg, MkListeners,
                     MsgType (..), NodeId, OutSpecs)
import           Seal.Infra.Communication.Relay (DataParams (..), Relay (..),
                     dataFlow, relayListeners, relayPropagateOut)
import           Seal.Infra.Network.Types (Bucket)
import           Seal.Logic.Types (Logic (..))
import           Seal.Util.Trace (Severity, Trace)

delegationListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
delegationListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (delegationRelays logic)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: Logic IO
    -> [Relay]
delegationRelays logic = [ pskHeavyRelay logic ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
delegationOutSpecs
    :: Logic IO
    -> OutSpecs
delegationOutSpecs logic = relayPropagateOut (delegationRelays logic)

pskHeavyRelay
    :: Logic IO
    -> Relay
pskHeavyRelay logic = Data $ DataParams
    MsgTransaction
    (\_ _ -> postPskHeavy logic)
    -- The message size limit for ProxySKHeavy: a ProxySecretKey with an
    -- EpochIndex.
    (pure (mlProxySecretKey mlHeavyDlgIndex))

sendPskHeavy
    :: Trace IO (Severity, Text)
    -> EnqueueMsg
    -> ProxySKHeavy
    -> IO ()
sendPskHeavy logTrace enqueue = dataFlow logTrace "pskHeavy" enqueue (MsgTransaction OQ.OriginSender)
