{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Server which handles transactions.
--
-- TODO rename this module. It doesn't define any listeners and doesn't deal
-- with a network.

module Seal.Listener.Txp
       ( handleTxDo
       , TxpMode
       ) where

import           Data.Tagged (Tagged (..))
import           Formatting (build, sformat, (%))
import           Node.Message.Class (Message)
import           Universum

import           Seal.Chain.Genesis as Genesis (Config)
import           Seal.Chain.Txp (TxAux (..), TxId, TxMsgContents (..),
                     TxpConfiguration)
import           Seal.Crypto (hash)
import           Seal.DB.Txp.MemState (MempoolExt, MonadTxpLocal, MonadTxpMem,
                     txpProcessTx)
import qualified Seal.Infra.Communication.Relay as Relay
import           Seal.Infra.Util.JsonLog.Events (JLEvent (..), JLTxR (..))
import           Seal.Util.Wlog (WithLogger, logInfo)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: TxpMode ctx m
    => Genesis.Config
    -> TxpConfiguration
    -> (JLEvent -> m ())  -- ^ How to log transactions
    -> TxAux              -- ^ Incoming transaction to be processed
    -> m Bool
handleTxDo genesisConfig txpConfig logTx txAux = do
    let txId = hash (taTx txAux)
    res <- txpProcessTx genesisConfig txpConfig (txId, txAux)
    let json me = logTx $ JLTxReceived $ JLTxR
            { jlrTxId  = sformat build txId
            , jlrError = me
            }
    case res of
        Right _ -> do
            logInfo $ sformat
                ("Transaction has been added to storage: " % build)
                txId
            json Nothing
            pure True
        Left er -> do
            logInfo $ sformat
                ( "Transaction hasn't been added to storage: "
                % build
                % " , reason: "
                % build
                )
                txId
                er
            json $ Just $ sformat build er
            pure False

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type TxpMode ctx m =
    ( MonadIO m
    , WithLogger m
    , MonadTxpLocal m
    , MonadTxpMem (MempoolExt m) ctx m
    , Each '[Message]
        '[ Relay.InvOrData (Tagged TxMsgContents TxId) TxMsgContents
         , Relay.InvMsg    (Tagged TxMsgContents TxId)
         , Relay.ReqOrRes  (Tagged TxMsgContents TxId)
         , Relay.ReqMsg    (Tagged TxMsgContents TxId)
         , Relay.MempoolMsg TxMsgContents
         ]
    )
