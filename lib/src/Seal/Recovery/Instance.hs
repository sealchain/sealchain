{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An instance of 'MonadRecoveryInfo'.

module Seal.Recovery.Instance
       (
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Monad.Except (runExceptT, throwError)

import           Seal.Block.BHelpers ()
import           Seal.Block.Types (RecoveryHeader, RecoveryHeaderTag)
import           Seal.Core (epochOrSlotG, epochOrSlotToSlot, flattenSlotId)
import qualified Seal.DB.BlockIndex as DB
import           Seal.DB.Class (MonadDBRead)
import           Seal.Infra.Recovery.Info (MonadRecoveryInfo (..),
                     SyncStatus (..))
import           Seal.Infra.Slotting (MonadSlots (getCurrentSlot))
import           Seal.Util.Util (HasLens (..))

instance ( Monad m
         , MonadIO m
         , MonadDBRead m
         , MonadSlots ctx m
         , MonadReader ctx m
         , HasLens RecoveryHeaderTag ctx RecoveryHeader
         ) =>
         MonadRecoveryInfo m where
    getSyncStatus epochSlots lagBehindParam =
        fmap convertRes . runExceptT $ do
            recoveryIsInProgress >>= \case
                False -> pass
                True -> throwError SSDoingRecovery
            curSlot <- note SSUnknownSlot =<< getCurrentSlot epochSlots
            tipHeader <- lift DB.getTipHeader
            let tipSlot = epochOrSlotToSlot (tipHeader ^. epochOrSlotG)
            unless (tipSlot <= curSlot) $
                throwError
                    SSInFuture
                    {sslbCurrentSlot = curSlot, sslbTipSlot = tipSlot}
            let slotDiff = flattenSlotId epochSlots curSlot
                    - flattenSlotId epochSlots tipSlot
            unless (slotDiff < fromIntegral lagBehindParam) $
                throwError
                    SSLagBehind
                    {sslbCurrentSlot = curSlot, sslbTipSlot = tipSlot}
      where
        recoveryIsInProgress = do
            var <- view (lensOf @RecoveryHeaderTag)
            isJust <$> atomically (STM.tryReadTMVar var)
        convertRes :: Either SyncStatus () -> SyncStatus
        convertRes (Left ss)  = ss
        convertRes (Right ()) = SSKindaSynced
