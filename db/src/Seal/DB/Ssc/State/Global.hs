{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Methods that operate on 'SscGlobalState' and 'VssCertificatesMap'.

module Seal.DB.Ssc.State.Global
       (
       -- * Certs
         getGlobalCerts
       , getStableCerts

       -- * Global state
       , sscLoadGlobalState
       , sscGetGlobalState
       ) where

import           Formatting (build, sformat, (%))
import           Universum

import           Seal.Chain.Genesis as Genesis (Config)
import           Seal.Chain.Ssc (MonadSscMem, SscGlobalState (..),
                     VssCertificatesMap (..), getStableCertsPure,
                     sgsVssCertificates, sscRunGlobalQuery)
import qualified Seal.Chain.Ssc as Ssc
import           Seal.Core (EpochIndex (..), SlotId (..))
import           Seal.DB (MonadDBRead)
import qualified Seal.DB.Ssc.GState as DB
import           Seal.Util.Wlog (WithLogger, logDebug, logInfo)

----------------------------------------------------------------------------
-- Certs
----------------------------------------------------------------------------

getGlobalCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        Ssc.certs .
        Ssc.setLastKnownSlot sl <$>
        view sgsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => Genesis.Config
    -> EpochIndex
    -> m VssCertificatesMap
getStableCerts genesisConfig epoch = getStableCertsPure genesisConfig epoch
    <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Load global state from DB by recreating it from recent blocks.
sscLoadGlobalState :: (MonadDBRead m, WithLogger m) => m SscGlobalState
sscLoadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo (sformat ("Loaded SSC state: " %build) gs)

sscGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
sscGetGlobalState = sscRunGlobalQuery ask
