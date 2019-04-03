{-# LANGUAGE RecordWildCards #-}

-- | Main Toss logic.

module Seal.Chain.Ssc.Toss.Logic
       ( verifyAndApplySscPayload
       , applyGenesisBlock
       , rollbackSsc
       , normalizeToss
       , refreshToss
       ) where

import           Universum hiding (id)

import           Control.Lens (at)
import           Control.Monad.Except (MonadError, runExceptT, throwError)
import           Crypto.Random (MonadRandom)
import qualified Data.HashMap.Strict as HM

import           Seal.Chain.Block.IsHeader (IsMainHeader, headerSlotL)
import           Seal.Chain.Genesis as Genesis (Config (..),
                     configSlotSecurityParam)
import           Seal.Chain.Ssc.Commitment (SignedCommitment)
import           Seal.Chain.Ssc.CommitmentsMap (CommitmentsMap (..),
                     getCommitmentsMap, mkCommitmentsMapUnsafe)
import           Seal.Chain.Ssc.Error (SscVerifyError (..))
import           Seal.Chain.Ssc.Functions (verifySscPayload)
import           Seal.Chain.Ssc.Opening (Opening)
import           Seal.Chain.Ssc.Payload (SscPayload (..), checkSscPayload, spVss)
import           Seal.Chain.Ssc.SharesMap (InnerSharesMap)
import           Seal.Chain.Ssc.Toss.Base (checkPayload)
import           Seal.Chain.Ssc.Toss.Class (MonadToss (..), MonadTossEnv (..))
import           Seal.Chain.Ssc.Toss.Types (TossModifier (..))
import           Seal.Chain.Ssc.VssCertificate (VssCertificate)
import           Seal.Chain.Ssc.VssCertificatesMap (getVssCertificatesMap,
                     mkVssCertificatesMapSingleton)
import           Seal.Core (EpochIndex, EpochOrSlot (..), LocalSlotIndex,
                     SlotCount, SlotId (siSlot), StakeholderId, Currency (mkMoney), 
                     epochIndexL, epochOrSlot, epochOrSlotPred, epochOrSlotToEnum,
                     getEpochOrSlot, getSlotIndex)
import           Seal.Core.Chrono (NewestFirst (..))
import           Seal.Util.AssertMode (inAssertMode)
import           Seal.Util.Some (Some)
import           Seal.Util.Util (sortWithMDesc)
import           Seal.Util.Wlog (logError)

-- | Verify 'SscPayload' with respect to data provided by
-- MonadToss. If data is valid it is also applied.  Otherwise
-- SscVerifyError is thrown using 'MonadError' type class.
verifyAndApplySscPayload
    :: ( MonadToss m
       , MonadTossEnv m
       , MonadError SscVerifyError m
       , MonadRandom m
       )
    => Genesis.Config
    -> Either EpochIndex (Some IsMainHeader)
    -> SscPayload
    -> m ()
verifyAndApplySscPayload genesisConfig eoh payload = do
    -- Check the payload for internal consistency.
    either (throwError . SscInvalidPayload)
           pure
           (checkSscPayload (configProtocolMagic genesisConfig) payload)
    -- We can't trust payload from mempool, so we must call
    -- @verifySscPayload@.
    whenLeft eoh $ const $ verifySscPayload genesisConfig eoh payload
    -- We perform @verifySscPayload@ for block when we construct it
    -- (in the 'recreateGenericBlock').  So this check is just in case.
    -- NOTE: in block verification `verifySscPayload` on `MainBlock` is run
    -- by `Seal.Block.BHelper.verifyMainBlock`
    inAssertMode $
        whenRight eoh $ const $ verifySscPayload genesisConfig eoh payload
    let blockCerts = spVss payload
    let curEpoch = either identity (^. epochIndexL) eoh
    checkPayload genesisConfig curEpoch payload

    -- Apply
    case eoh of
        Left _       -> pass
        Right header -> do
            let eos = EpochOrSlot $ Right $ header ^. headerSlotL
            setEpochOrSlot eos
            -- We can freely clear shares after 'slotSecurityParam' because
            -- it's guaranteed that rollback on more than 'slotSecurityParam'
            -- can't happen
            let indexToCount :: LocalSlotIndex -> SlotCount
                indexToCount = fromIntegral . getSlotIndex
            let slot = epochOrSlot (const 0) (indexToCount . siSlot) eos
                slotSecurityParam = configSlotSecurityParam genesisConfig
            when (slotSecurityParam <= slot && slot < 2 * slotSecurityParam) $
                resetShares
    mapM_ putCertificate blockCerts
    case payload of
        CommitmentsPayload  comms  _ ->
            mapM_ putCommitment $ toList $ getCommitmentsMap comms
        OpeningsPayload     opens  _ ->
            mapM_ (uncurry putOpening) $ HM.toList opens
        SharesPayload       shares _ ->
            mapM_ (uncurry putShares) $ HM.toList shares
        CertificatesPayload        _ ->
            pass

-- | Apply genesis block for given epoch to 'Toss' state.
applyGenesisBlock :: MonadToss m => EpochIndex -> m ()
applyGenesisBlock epoch = do
    setEpochOrSlot $ getEpochOrSlot epoch
    -- We don't clear shares on genesis block because
    -- there aren't 'slotSecurityParam' slots after shares phase,
    -- so we won't have shares after rollback
    -- We store shares until 'slotSecurityParam' slots of next epoch passed
    -- and clear their after that
    resetCO

-- | Rollback application of 'SscPayload's in 'Toss'. First argument is
-- 'EpochOrSlot' of oldest block which is subject to rollback.
rollbackSsc
    :: MonadToss m
    => SlotCount
    -> EpochOrSlot
    -> NewestFirst [] SscPayload
    -> m ()
rollbackSsc epochSlots oldestEOS (NewestFirst payloads)
    | oldestEOS == epochOrSlotToEnum epochSlots 0 = do
        logError "rollbackSsc: most genesis block is passed to rollback"
        setEpochOrSlot oldestEOS
        resetCO
        resetShares
    | otherwise = do
        setEpochOrSlot (epochOrSlotPred epochSlots oldestEOS)
        mapM_ rollbackSscDo payloads
  where
    rollbackSscDo (CommitmentsPayload comms _) =
        mapM_ delCommitment $ HM.keys $ getCommitmentsMap comms
    rollbackSscDo (OpeningsPayload opens _) = mapM_ delOpening $ HM.keys opens
    rollbackSscDo (SharesPayload shares _) = mapM_ delShares $ HM.keys shares
    rollbackSscDo (CertificatesPayload _) = pass

-- | Apply as much data from given 'TossModifier' as possible.
normalizeToss
    :: (MonadToss m, MonadTossEnv m, MonadRandom m)
    => Genesis.Config
    -> EpochIndex
    -> TossModifier
    -> m ()
normalizeToss genesisConfig epoch TossModifier {..} =
    normalizeTossDo
        genesisConfig
        epoch
        ( HM.toList (getCommitmentsMap _tmCommitments)
        , HM.toList _tmOpenings
        , HM.toList _tmShares
        , HM.toList (getVssCertificatesMap _tmCertificates))

-- | Apply the most valuable from given 'TossModifier' and drop the
-- rest. This function can be used if mempool is exhausted.
refreshToss
    :: (MonadToss m, MonadTossEnv m, MonadRandom m)
    => Genesis.Config
    -> EpochIndex
    -> TossModifier
    -> m ()
refreshToss genesisConfig epoch TossModifier {..} = do
    comms <-
        takeMostValuable epoch (HM.toList (getCommitmentsMap _tmCommitments))
    opens <- takeMostValuable epoch (HM.toList _tmOpenings)
    shares <- takeMostValuable epoch (HM.toList _tmShares)
    certs <- takeMostValuable epoch (HM.toList (getVssCertificatesMap _tmCertificates))
    normalizeTossDo genesisConfig epoch (comms, opens, shares, certs)

takeMostValuable
    :: (MonadToss m, MonadTossEnv m)
    => EpochIndex
    -> [(StakeholderId, x)]
    -> m [(StakeholderId, x)]
takeMostValuable epoch items = take toTake <$> sortWithMDesc resolver items
  where
    toTake = 2 * length items `div` 3
    resolver (id, _) =
        fromMaybe (mkMoney 0) . (view (at id)) . fromMaybe mempty <$>
        getRichmen epoch

type TossModifierLists
     = ( [(StakeholderId, SignedCommitment)]
       , [(StakeholderId, Opening)]
       , [(StakeholderId, InnerSharesMap)]
       , [(StakeholderId, VssCertificate)])

normalizeTossDo
    :: forall m
     . (MonadToss m, MonadTossEnv m, MonadRandom m)
    => Genesis.Config
    -> EpochIndex
    -> TossModifierLists
    -> m ()
normalizeTossDo genesisConfig epoch (comms, opens, shares, certs) = do
    putsUseful $
        map (flip CommitmentsPayload mempty . mkCommitmentsMapUnsafe . one) $
        comms
    putsUseful $ map (flip OpeningsPayload mempty . one) opens
    putsUseful $ map (flip SharesPayload mempty . one) shares
    putsUseful $ map (CertificatesPayload . mkVssCertificatesMapSingleton . snd) certs
  where
    putsUseful :: [SscPayload] -> m ()
    putsUseful entries = do
        let verifyAndApply = runExceptT . verifyAndApplySscPayload genesisConfig
                                                                   (Left epoch)
        mapM_ verifyAndApply entries
