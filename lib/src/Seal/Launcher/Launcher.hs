{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Applications of runners to scenarios.

module Seal.Launcher.Launcher
    ( -- * Node launcher.
      launchNode

     -- * Actions
    , actionWithCoreNode
    ) where

import           Universum

import           Ntp.Client (NtpConfiguration)
import           Seal.Chain.Genesis as Genesis (Config (..))
import           Seal.Chain.Ssc (SscParams)
import           Seal.Chain.Txp (TxpConfiguration)
import           Seal.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Seal.Client.CLI.Options (configurationOptions)
import           Seal.Client.CLI.Params (getNodeParams)
import           Seal.DB.DB (initNodeDBs)
import           Seal.DB.Txp.Logic (txpGlobalSettings)
import           Seal.Launcher.Configuration (AssetLockPath (..),
                     HasConfigurations, WalletConfiguration, cfoKey,
                     withConfigurations)
import           Seal.Launcher.Param (LoggingParams (..), NodeParams (..))
import           Seal.Launcher.Resource (NodeResources, bracketNodeResources,
                     loggerBracket)
import           Seal.Launcher.Runner (runRealMode)
import           Seal.Launcher.Scenario (runNode)
import           Seal.Util.CompileInfo (HasCompileInfo)
import           Seal.Util.Util (logException)
import           Seal.Util.Wlog (logInfo)
import           Seal.Worker.Update (updateTriggerWorker)
import           Seal.WorkMode (EmptyMempoolExt)


-- | Run a given action from a bunch of static arguments
launchNode
    :: NodeArgs
    -> CommonNodeArgs
    -> LoggingParams
    -> (  HasConfigurations
       => Genesis.Config
       -> WalletConfiguration
       -> TxpConfiguration
       -> NtpConfiguration
       -> NodeParams
       -> SscParams
       -> NodeResources EmptyMempoolExt
       -> IO ()
       )
    -> IO ()
launchNode nArgs cArgs lArgs action = do
    let confOpts = configurationOptions (commonArgs cArgs)
    let confKey = cfoKey confOpts
    let withLogger' = loggerBracket confKey lArgs . logException (lpDefaultName lArgs)
    let withConfigurations' = withConfigurations
            (AssetLockPath <$> cnaAssetLockPath cArgs)
            (cnaDumpGenesisDataPath cArgs)
            (cnaDumpConfiguration cArgs)
            confOpts

    withLogger' $ withConfigurations' $ \genesisConfig walletConfig txpConfig ntpConfig -> do
        (nodeParams, Just sscParams) <- getNodeParams
            (lpDefaultName lArgs)
            cArgs
            nArgs
            (configGeneratedSecrets genesisConfig)

        let action' = action
                genesisConfig
                walletConfig
                txpConfig
                ntpConfig
                nodeParams
                sscParams

        bracketNodeResources
            genesisConfig
            nodeParams
            sscParams
            (txpGlobalSettings genesisConfig txpConfig)
            (initNodeDBs genesisConfig)
            action'


-- | Run basic core node
actionWithCoreNode
    :: (HasConfigurations, HasCompileInfo)
    => Genesis.Config
    -> WalletConfiguration
    -> TxpConfiguration
    -> NtpConfiguration
    -> NodeParams
    -> SscParams
    -> NodeResources EmptyMempoolExt
    -> IO ()
actionWithCoreNode genesisConfig _ txpConfig _ _ _ nodeRes = do
    let plugins = [ ("update trigger", updateTriggerWorker) ]

    logInfo "Wallet is disabled, because software is built w/o it"

    runRealMode
        genesisConfig
        txpConfig
        nodeRes
        (runNode genesisConfig txpConfig nodeRes plugins)
