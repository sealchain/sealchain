{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seal.Logic.Full
    ( LogicWorkMode
    , logicFull
    ) where

import           Universum hiding (id)

import           Control.Lens (at, to)
import           Data.Conduit (ConduitT)
import qualified Data.HashMap.Strict as HM
import           Data.Tagged (Tagged (..), tagWith)
import           Formatting (build, sformat, (%))

import           Seal.Chain.Block (Block, BlockHeader, HasBlockConfiguration,
                     HeaderHash)
import           Seal.Chain.Delegation (ProxySKHeavy)
import           Seal.Chain.Genesis as Genesis (Config (..),
                     configBlkSecurityParam, configEpochSlots)
import           Seal.Chain.Security (SecurityParams, shouldIgnorePkAddress)
import           Seal.Chain.Ssc (MCCommitment (..), MCOpening (..),
                     MCShares (..), MCVssCertificate (..), SscTag (..),
                     TossModifier, getCertId, getCommitmentsMap, ldModifier,
                     lookupVss, sscRunLocalQuery, tmCertificates,
                     tmCommitments, tmOpenings, tmShares)
import           Seal.Chain.Txp (MemPool (..), TxAux (..), TxMsgContents (..),
                     TxpConfiguration)
import           Seal.Chain.Update (BlockVersionData, UpdateProposal (..),
                     UpdateVote (..))
import           Seal.Communication (NodeId)
import           Seal.Core (StakeholderId, addressHash)
import           Seal.Core.Chrono (NE, NewestFirst, OldestFirst)
import           Seal.Crypto (hash)
import qualified Seal.DB.Block as Block
import qualified Seal.DB.Block as DB (getTipBlock)
import qualified Seal.DB.BlockIndex as DB (getHeader, getTipHeader)
import           Seal.DB.Class (MonadBlockDBRead, MonadDBRead, MonadGState (..),
                     SerializedBlock)
import qualified Seal.DB.Class as DB (MonadDBRead (dbGetSerBlock))
import           Seal.DB.Ssc (sscIsDataUseful, sscProcessCertificate,
                     sscProcessCommitment, sscProcessOpening, sscProcessShares)
import           Seal.DB.Txp.MemState (getMemPool, withTxpLocalData)
import           Seal.DB.Update (getLocalProposalNVotes, getLocalVote,
                     isProposalNeeded, isVoteNeeded)
import           Seal.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Seal.Infra.Slotting (MonadSlots)
import           Seal.Infra.Util.JsonLog.Events (JLEvent)
import           Seal.Listener.Delegation (DlgListenerConstraint)
import qualified Seal.Listener.Delegation as Delegation (handlePsk)
import           Seal.Listener.Txp (TxpMode)
import qualified Seal.Listener.Txp as Txp (handleTxDo)
import           Seal.Listener.Update (UpdateMode, handleProposal, handleVote)
import           Seal.Logic.Types (KeyVal (..), Logic (..))
import qualified Seal.Network.Block.Logic as Block
import           Seal.Network.Block.WorkMode (BlockWorkMode)
import           Seal.Recovery (MonadRecoveryInfo)
import qualified Seal.Recovery as Recovery
import           Seal.Util.Util (HasLens (..))
import           Seal.Util.Wlog (WithLogger, logDebug)

-- The full logic layer uses existing pieces from the former monolithic
-- approach, in which there was no distinction between networking and
-- blockchain logic (any piece could use the network via some class constraint
-- on the monad). The class-based approach is not problematic for
-- integration with a diffusion layer, because in practice the concrete
-- monad which satisfies these constraints is a reader form over IO, so we
-- always have
--
--   runIO  :: m x -> IO x
--   liftIO :: IO x -> m x
--
-- thus a diffusion layer which is in IO can be made to work with a logic
-- layer which uses the more complicated monad, and vice-versa.

type LogicWorkMode ctx m =
    ( HasBlockConfiguration
    , WithLogger m
    , MonadReader ctx m
    , MonadMask m
    , MonadBlockDBRead m
    , MonadDBRead m
    , MonadGState m
    , MonadRecoveryInfo ctx m
    , MonadSlots ctx m
    , HasLens RecoveryHeaderTag ctx RecoveryHeader
    , BlockWorkMode ctx m
    , DlgListenerConstraint ctx m
    , TxpMode ctx m
    , UpdateMode ctx m
    )

-- | A stop-gap full logic layer based on the RealMode. It just uses the
-- monadX constraints to do most of its work.
logicFull
    :: forall ctx m .
       LogicWorkMode ctx m
    => Genesis.Config
    -> TxpConfiguration
    -> StakeholderId
    -> SecurityParams
    -> (JLEvent -> m ()) -- ^ JSON log callback. FIXME replace by structured logging solution
    -> Logic m
logicFull genesisConfig txpConfig ourStakeholderId securityParams jsonLogTx =
    let
        genesisHash = configGenesisHash genesisConfig

        getSerializedBlock :: HeaderHash -> m (Maybe SerializedBlock)
        getSerializedBlock = DB.dbGetSerBlock genesisHash

        streamBlocks :: HeaderHash -> ConduitT () SerializedBlock m ()
        streamBlocks = Block.streamBlocks (DB.dbGetSerBlock genesisHash)
                                          Block.resolveForwardLink

        getTip :: m Block
        getTip = DB.getTipBlock genesisHash

        getTipHeader :: m BlockHeader
        getTipHeader = DB.getTipHeader

        getAdoptedBVData :: m BlockVersionData
        getAdoptedBVData = gsAdoptedBVData

        recoveryInProgress :: m Bool
        recoveryInProgress =
            Recovery.recoveryInProgress $ configEpochSlots genesisConfig

        getBlockHeader :: HeaderHash -> m (Maybe BlockHeader)
        getBlockHeader = DB.getHeader

        getHashesRange
            :: Maybe Word -- ^ Optional limit on how many to pull in.
            -> HeaderHash
            -> HeaderHash
            -> m (Either Block.GetHashesRangeError (OldestFirst NE HeaderHash))
        getHashesRange = Block.getHashesRange

        getBlockHeaders
            :: Maybe Word -- ^ Optional limit on how many to pull in.
            -> NonEmpty HeaderHash
            -> Maybe HeaderHash
            -> m
                   ( Either
                         Block.GetHeadersFromManyToError
                         (NewestFirst NE BlockHeader)
                   )
        getBlockHeaders = Block.getHeadersFromManyTo

        getLcaMainChain
            :: OldestFirst [] HeaderHash
            -> m (NewestFirst [] HeaderHash, OldestFirst [] HeaderHash)
        getLcaMainChain = Block.lcaWithMainChainSuffix

        postBlockHeader :: BlockHeader -> NodeId -> m ()
        postBlockHeader = Block.handleUnsolicitedHeader genesisConfig

        postPskHeavy :: ProxySKHeavy -> m Bool
        postPskHeavy = Delegation.handlePsk genesisConfig

        postTx = KeyVal
            { toKey = pure . Tagged . hash . taTx . getTxMsgContents
            , handleInv = \(Tagged txId) -> not . HM.member txId . _mpLocalTxs <$> withTxpLocalData getMemPool
            , handleReq = \(Tagged txId) -> fmap TxMsgContents . HM.lookup txId . _mpLocalTxs <$> withTxpLocalData getMemPool
            , handleData = \(TxMsgContents txAux) -> Txp.handleTxDo genesisConfig txpConfig jsonLogTx txAux
            }

        postUpdate = KeyVal
            { toKey = \(up, _) -> pure . tag $ hash up
            , handleInv = isProposalNeeded . unTagged
            , handleReq = getLocalProposalNVotes . unTagged
            , handleData = handleProposal genesisConfig
            }
          where
            tag = tagWith (Proxy :: Proxy (UpdateProposal, [UpdateVote]))

        postVote = KeyVal
            { toKey = \UnsafeUpdateVote{..} -> pure $ tag (uvProposalId, uvKey, uvDecision)
            , handleInv = \(Tagged (id, pk, dec)) -> isVoteNeeded id pk dec
            , handleReq = \(Tagged (id, pk, dec)) -> getLocalVote id pk dec
            , handleData = handleVote genesisConfig
            }
          where
            tag = tagWith (Proxy :: Proxy UpdateVote)

        postSscCommitment = postSscCommon
            CommitmentMsg
            (\(MCCommitment (pk, _, _)) -> addressHash pk)
            (\id tm -> MCCommitment <$> tm ^. tmCommitments . to getCommitmentsMap . at id)
            (\(MCCommitment comm) -> sscProcessCommitment genesisConfig comm)

        postSscOpening = postSscCommon
            OpeningMsg
            (\(MCOpening key _) -> key)
            (\id tm -> MCOpening id <$> tm ^. tmOpenings . at id)
            (\(MCOpening key open) -> sscProcessOpening genesisConfig key open)

        postSscShares = postSscCommon
            SharesMsg
            (\(MCShares key _) -> key)
            (\id tm -> MCShares id <$> tm ^. tmShares . at id)
            (\(MCShares key shares) -> sscProcessShares genesisConfig key shares)

        postSscVssCert = postSscCommon
            VssCertificateMsg
            (\(MCVssCertificate vc) -> getCertId vc)
            (\id tm -> MCVssCertificate <$> lookupVss id (tm ^. tmCertificates))
            (\(MCVssCertificate cert) -> sscProcessCertificate genesisConfig cert)

        postSscCommon
            :: (Buildable err, Buildable contents)
            => SscTag
            -> (contents -> StakeholderId)
            -> (StakeholderId -> TossModifier -> Maybe contents)
            -> (contents -> m (Either err ()))
            -> KeyVal (Tagged contents StakeholderId) contents m
        postSscCommon sscTag contentsToKey toContents processData = KeyVal
            { toKey = pure . tagWith contentsProxy . contentsToKey
            , handleInv =
                  sscIsDataUseful (configBlkSecurityParam genesisConfig) sscTag
                      . unTagged
            , handleReq = \(Tagged addr) -> toContents addr . view ldModifier <$> sscRunLocalQuery ask
            , handleData = \dat -> do
                  let addr = contentsToKey dat
                  -- [CSL-685] TODO: Add here malicious emulation for network
                  -- addresses when TW will support getting peer address
                  -- properly
                  -- Stale comment?
                  handleDataDo dat addr =<< shouldIgnorePkAddress addr
            }
          where
            contentsProxy = (const Proxy :: (contents -> k) -> Proxy contents) contentsToKey
            ignoreFmt =
                "Malicious emulation: data "%build%" for id "%build%" is ignored"
            handleDataDo dat id shouldIgnore
                | shouldIgnore = False <$ logDebug (sformat ignoreFmt id dat)
                | otherwise = sscProcessMessage processData dat
            sscProcessMessage sscProcessMessageDo dat =
                sscProcessMessageDo dat >>= \case
                    Left err -> False <$ logDebug (sformat ("Data is rejected, reason: "%build) err)
                    Right () -> return True
    in Logic {..}
