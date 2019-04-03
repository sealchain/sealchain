{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

-- | Runners in various modes.

module Seal.Launcher.Runner
       ( -- * High level runners
         runRealMode

       -- * Exported for custom usage in CLI utils
       , runServer
       ) where

import           Universum

import           Control.Concurrent.Async (race)
import qualified Control.Monad.Reader as Mtl
import           Data.Default (Default)
import           System.Exit (ExitCode (..))

import           Seal.Behavior (bcSecurityParams)
import           Seal.Binary ()
import           Seal.Chain.Block (HasBlockConfiguration, recoveryHeadersMessage,
                     streamWindow)
import           Seal.Chain.Genesis as Genesis (Config (..))
import           Seal.Chain.Txp (TxpConfiguration)
import           Seal.Chain.Update (HasUpdateConfiguration,
                     lastKnownBlockVersion)
import           Seal.Configuration (HasNodeConfiguration,
                     networkConnectionTimeout)
import           Seal.Context.Context (NodeContext (..))
import           Seal.Core (StakeholderId, addressHash)
import           Seal.Core.JsonLog (jsonLog)
import           Seal.Crypto (ProtocolMagic, toPublic)
import           Seal.DB.Txp (MonadTxpLocal)
import           Seal.Diffusion.Full (FullDiffusionConfiguration (..),
                     diffusionLayerFull)
import           Seal.Infra.Diffusion.Types (Diffusion (..), DiffusionLayer (..),
                     hoistDiffusion)
import           Seal.Infra.InjectFail (FInject (..), testLogFInject)
import           Seal.Infra.Network.Types (NetworkConfig (..),
                     topologyRoute53HealthCheckEnabled)
import           Seal.Infra.Reporting.Ekg (EkgNodeMetrics (..),
                     registerEkgMetrics, withEkgServer)
import           Seal.Infra.Reporting.Statsd (withStatsd)
import           Seal.Infra.Shutdown (ShutdownContext, waitForShutdown)
import           Seal.Launcher.Configuration (HasConfigurations)
import           Seal.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Seal.Launcher.Resource (NodeResources (..))
import           Seal.Logic.Full (logicFull)
import           Seal.Logic.Types (Logic, hoistLogic)
import           Seal.Reporting.Production (ProductionReporterParams (..),
                     productionReporter)
import           Seal.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Seal.Util.Trace (wlogTrace)
import           Seal.Web.Server (withRoute53HealthCheckApplication)
import           Seal.WorkMode (RealMode, RealModeContext (..))

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in something convertible to 'RealMode' and back.
runRealMode
    :: forall ext a.
       ( Default ext
       , HasCompileInfo
       , HasConfigurations
       , MonadTxpLocal (RealMode ext)
       -- MonadTxpLocal is meh,
       -- we can't remove @ext@ from @RealMode@ because
       -- explorer and wallet use RealMode,
       -- though they should use only @RealModeContext@
       )
    => Genesis.Config
    -> TxpConfiguration
    -> NodeResources ext
    -> (Diffusion (RealMode ext) -> RealMode ext a)
    -> IO a
runRealMode genesisConfig txpConfig nr@NodeResources {..} act = runServer
    genesisConfig
    ncNodeParams
    (EkgNodeMetrics nrEkgStore)
    ncShutdownContext
    makeLogicIO
    act'
  where
    NodeContext {..} = nrContext
    NodeParams {..}  = ncNodeParams
    securityParams   = bcSecurityParams npBehaviorConfig
    ourStakeholderId :: StakeholderId
    ourStakeholderId = addressHash (toPublic npSecretKey)
    logic :: Logic (RealMode ext)
    logic = logicFull genesisConfig txpConfig ourStakeholderId securityParams jsonLog
    pm = configProtocolMagic genesisConfig
    makeLogicIO :: Diffusion IO -> Logic IO
    makeLogicIO diffusion = hoistLogic (elimRealMode pm nr diffusion) logic
    act' :: Diffusion IO -> IO a
    act' diffusion =
        let diffusion' = hoistDiffusion liftIO (elimRealMode pm nr diffusion) diffusion
         in elimRealMode pm nr diffusion (act diffusion')

-- | RealMode runner: creates a JSON log configuration and uses the
-- resources provided to eliminate the RealMode, yielding an IO.
elimRealMode
    :: forall t ext
     . HasCompileInfo
    => ProtocolMagic
    -> NodeResources ext
    -> Diffusion IO
    -> RealMode ext t
    -> IO t
elimRealMode pm NodeResources {..} diffusion action = do
    Mtl.runReaderT action (rmc nrJsonLogConfig)
  where
    NodeContext {..} = nrContext
    NodeParams {..} = ncNodeParams
    NetworkConfig {..} = ncNetworkConfig
    LoggingParams {..} = bpLoggingParams npBaseParams
    reporterParams = ProductionReporterParams
        { prpServers         = npReportServers
        , prpLoggerConfig    = ncLoggerConfig
        , prpCompileTimeInfo = compileInfo
        , prpTrace           = wlogTrace "reporter"
        , prpProtocolMagic   = pm
        }
    rmc jlConf = RealModeContext
        nrDBs
        nrSscState
        nrTxpState
        nrDlgState
        jlConf
        lpDefaultName
        nrContext
        (productionReporter reporterParams diffusion)

-- | "Batteries-included" server.
-- Bring up a full diffusion layer over a TCP transport and use it to run some
-- action. Also brings up ekg monitoring, route53 health check, statds,
-- according to parameters.
-- Uses magic Data.Reflection configuration for the protocol constants,
-- network connection timeout (nt-tcp), and, and the 'recoveryHeadersMessage'
-- number.
runServer
    :: forall t
     . (HasBlockConfiguration, HasNodeConfiguration, HasUpdateConfiguration)
    => Genesis.Config
    -> NodeParams
    -> EkgNodeMetrics
    -> ShutdownContext
    -> (Diffusion IO -> Logic IO)
    -> (Diffusion IO -> IO t)
    -> IO t
runServer genesisConfig NodeParams {..} ekgNodeMetrics shdnContext mkLogic act = exitOnShutdown npFInjects $
    diffusionLayerFull fdconf
                       npNetworkConfig
                       (Just ekgNodeMetrics)
                       mkLogic $ \diffusionLayer -> do
        when npEnableMetrics (registerEkgMetrics ekgStore)
        runDiffusionLayer diffusionLayer $
            maybeWithRoute53 (healthStatus (diffusion diffusionLayer)) $
            maybeWithEkg $
            maybeWithStatsd $
            -- The 'act' is in 'm', and needs a 'Diffusion m'. We can hoist
            -- that, since 'm' is 'MonadIO'.
            (act (diffusion diffusionLayer))

  where
    fdconf = FullDiffusionConfiguration
        { fdcProtocolMagic = configProtocolMagic genesisConfig
        , fdcProtocolConstants = configProtocolConstants genesisConfig
        , fdcRecoveryHeadersMessage = recoveryHeadersMessage
        , fdcLastKnownBlockVersion = lastKnownBlockVersion
        , fdcConvEstablishTimeout = networkConnectionTimeout
        , fdcTrace = wlogTrace "diffusion"
        , fdcStreamWindow = streamWindow
        }
    exitOnShutdown fInjects action = do
        _ <- race (waitForShutdown shdnContext) action
        doFail <- testLogFInject fInjects FInjApplyUpdateWrongExitCode
        exitWith $ ExitFailure $
          if doFail
          then 42 -- inject wrong exit code
          else 20 -- special exit code to indicate an update
    ekgStore = enmStore ekgNodeMetrics
    (hcHost, hcPort) = case npRoute53Params of
        Nothing         -> ("127.0.0.1", 3030)
        Just (hst, prt) -> (decodeUtf8 hst, fromIntegral prt)
    maybeWithRoute53 mStatus = case topologyRoute53HealthCheckEnabled (ncTopology npNetworkConfig) of
        True  -> withRoute53HealthCheckApplication mStatus hcHost hcPort
        False -> identity
    maybeWithEkg = case (npEnableMetrics, npEkgParams) of
        (True, Just ekgParams) -> withEkgServer ekgParams ekgStore
        _                      -> identity
    maybeWithStatsd = case (npEnableMetrics, npStatsdParams) of
        (True, Just sdParams) -> withStatsd sdParams ekgStore
        _                     -> identity
