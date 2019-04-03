{-# LANGUAGE Rank2Types #-}

-- | Protocol/versioning related communication helpers.

module Seal.Infra.Communication.Listener
       ( listenerConv
       ) where

import qualified Node as N
import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ
import           Seal.Binary.Class (Bi)
import           Seal.Infra.Communication.Protocol (ConversationActions,
                     HandlerSpec (..), ListenerSpec (..), Message, NodeId,
                     OutSpecs, VerInfo (..), checkProtocolMagic,
                     checkingInSpecs, messageCode)
import           Seal.Infra.Network.Types (Bucket)
import           Seal.Util.Trace (Severity, Trace)

-- TODO automatically provide a 'recvLimited' here by using the
-- 'MessageLimited'?
listenerConv
    :: forall snd rcv pack .
       ( Bi snd
       , Bi rcv
       , Message snd
       , Message rcv
       )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> (VerInfo -> NodeId -> ConversationActions snd rcv -> IO ())
    -> (ListenerSpec, OutSpecs)
listenerConv logTrace oq h = (lspec, mempty)
  where
    spec = (rcvMsgCode, ConvHandler sndMsgCode)
    lspec =
      flip ListenerSpec spec $ \ourVerInfo ->
          N.Listener $ \peerVerInfo' nNodeId conv -> checkProtocolMagic ourVerInfo peerVerInfo' $ do
              OQ.clearFailureOf oq nNodeId
              checkingInSpecs logTrace ourVerInfo peerVerInfo' spec nNodeId $
                  h ourVerInfo nNodeId conv

    sndProxy :: Proxy snd
    sndProxy = Proxy
    rcvProxy :: Proxy rcv
    rcvProxy = Proxy

    sndMsgCode = messageCode sndProxy
    rcvMsgCode = messageCode rcvProxy
