{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Seal.Web.Api
       ( NodeApi
       , nodeApi

       , HealthCheckApi
       , healthCheckApi
       ) where

import           Universum

import           Servant.API ((:<|>), (:>), Capture, Get, JSON, PlainText, Post,
                     QueryParam)

import           Seal.Chain.Block (HeaderHash)
import           Seal.Chain.Txp (TxOut)
import           Seal.Core (EpochIndex, SlotLeaders)
import           Seal.Crypto (PublicKey)
import           Seal.Web.Types (CConfirmedProposalState)

----------------------------------------------------------------------------
-- Base
----------------------------------------------------------------------------

-- | Servant API which provides access to full node internals.
--
-- Implementations of these methods are in
-- 'Seal.Web.Server.nodeServantHandlers'.
type NodeApi =
    -- "current_slot"
    --     :> Get '[JSON] SlotId
    -- :<|>
    "leaders"
        :> QueryParam "epoch" EpochIndex
        :> Get '[JSON] SlotLeaders
    :<|>
    "utxo"
        :> Get '[JSON] [TxOut]
    :<|>
    "spending_key"
        :> Get '[JSON] PublicKey
    :<|>
    "head_hash"
        :> Get '[JSON] HeaderHash
    :<|>
    "local_txs_num"
        :> Get '[JSON] Word
    :<|>
    "confirmed_proposals"
        :> Get '[JSON] [CConfirmedProposalState]
    :<|>
    "ssc" :>
        ("toggle"
            :> Capture "enable" Bool
            :> Post '[JSON] ()
         -- :<|>
         -- "has_secret" :> Get '[JSON] Bool :<|>
         -- "secret" :> Get '[JSON] SharedSeed :<|>
         -- "stage" :> Get '[JSON] SscStage
        )

-- | Helper Proxy.
nodeApi :: Proxy NodeApi
nodeApi = Proxy

----------------------------------------------------------------------------
-- HealthCheck
----------------------------------------------------------------------------

-- | Helper Proxy.
healthCheckApi :: Proxy HealthCheckApi
healthCheckApi = Proxy

type HealthCheckApi =
    "healthcheck" :> "route53" :> Get '[PlainText] String
