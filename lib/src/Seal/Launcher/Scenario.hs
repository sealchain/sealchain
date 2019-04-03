{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | High-level scenarios which can be launched.

module Seal.Launcher.Scenario
       ( runNode
       , runNode'
       , nodeStartMsg
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Formatting (bprint, build, int, sformat, shown, (%))
import           Serokell.Util (listJson)

import           Seal.Chain.Genesis as Genesis (Config (..),
                     GenesisDelegation (..), GenesisWStakeholders (..),
                     configBootStakeholders, configFtsSeed,
                     configHeavyDelegation)
import           Seal.Chain.Txp (TxpConfiguration, bootDustThreshold)
import           Seal.Chain.Update (HasUpdateConfiguration, curSoftwareVersion,
                     lastKnownBlockVersion, ourSystemTag)
import           Seal.Context (getOurPublicKey)
import           Seal.Core (addressHash)
import           Seal.Core.Conc (mapConcurrently)
import           Seal.Crypto (pskDelegatePk)
import qualified Seal.DB.BlockIndex as DB
import qualified Seal.GState as GS
import           Seal.Infra.Diffusion.Types (Diffusion)
import           Seal.Infra.Reporting (reportError)
import           Seal.Infra.Slotting (waitSystemStart)
import           Seal.Infra.Util.LogSafe (logInfoS)
import           Seal.Launcher.Resource (NodeResources (..))
import           Seal.Util.AssertMode (inAssertMode)
import           Seal.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Seal.Util.Wlog (WithLogger, askLoggerName, logInfo)
import           Seal.Worker (allWorkers)
import           Seal.WorkMode.Class (WorkMode)

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode'
    :: forall ext ctx m.
       ( HasCompileInfo
       , WorkMode ctx m
       )
    => Genesis.Config
    -> NodeResources ext
    -> [ (Text, Diffusion m -> m ()) ]
    -> [ (Text, Diffusion m -> m ()) ]
    -> Diffusion m -> m ()
runNode' genesisConfig NodeResources {..} workers' plugins' = \diffusion -> do
    logInfo $ "Built with: " <> pretty compileInfo
    nodeStartMsg
    inAssertMode $ logInfo "Assert mode on"
    pk <- getOurPublicKey
    let pkHash = addressHash pk
    logInfoS $ sformat ("My public key is: "%build%", pk hash: "%build)
        pk pkHash

    let genesisStakeholders = configBootStakeholders genesisConfig
    logInfo $ sformat
        ("Genesis stakeholders ("%int%" addresses, dust threshold "%build%"): "%build)
        (length $ getGenesisWStakeholders genesisStakeholders)
        (bootDustThreshold genesisStakeholders)
        genesisStakeholders

    let genesisDelegation = configHeavyDelegation genesisConfig
    let formatDlgPair (issuerId, delegateId) =
            bprint (build%" -> "%build) issuerId delegateId
    logInfo $ sformat ("GenesisDelegation (stakeholder ids): "%listJson)
            $ map (formatDlgPair . second (addressHash . pskDelegatePk))
            $ HM.toList
            $ unGenesisDelegation genesisDelegation

    firstGenesisHash <- GS.getFirstGenesisBlockHash $ configGenesisHash
        genesisConfig
    logInfo $ sformat
        ("First genesis block hash: "%build%", genesis seed is "%build)
        firstGenesisHash
        (configFtsSeed genesisConfig)

    tipHeader <- DB.getTipHeader
    logInfo $ sformat ("Current tip header: "%build) tipHeader

    waitSystemStart
    let
      runWithReportHandler :: (Text, Diffusion m -> m ()) -> m ()
      runWithReportHandler (workerName, action) = action diffusion `catch` (reportHandler workerName)

    void (mapConcurrently runWithReportHandler (workers' ++ plugins'))

    exitFailure
  where
    reportHandler :: Text -> SomeException -> m b
    reportHandler action (SomeException e) = do
        loggerName <- askLoggerName
        let msg = "Worker/plugin with work name "%shown%" and logger name "%shown%" failed with exception: "%shown
        reportError $ sformat msg action loggerName e
        exitFailure

-- | Entry point of full node.
-- Initialization, running of workers, running of plugins.
runNode
    :: ( HasCompileInfo
       , WorkMode ctx m
       )
    => Genesis.Config
    -> TxpConfiguration
    -> NodeResources ext
    -> [ (Text, Diffusion m -> m ()) ]
    -> Diffusion m -> m ()
runNode genesisConfig txpConfig nr plugins =
    runNode' genesisConfig nr workers' plugins
    where workers' = allWorkers genesisConfig txpConfig nr

-- | This function prints a very useful message when node is started.
nodeStartMsg :: (HasUpdateConfiguration, WithLogger m) => m ()
nodeStartMsg = logInfo msg
  where
    msg = sformat ("Application: " %build% ", last known block version "
                    %build% ", systemTag: " %build)
                   curSoftwareVersion
                   lastKnownBlockVersion
                   ourSystemTag
