-- | Definition of a 'Reporter IO' which uses log-warper to gather logs and
-- uses the HTTP backend to send them to some server(s).

module Seal.Reporting.Production
    ( ProductionReporterParams (..)
    , productionReporter
    ) where

import           Universum

import           Control.Exception.Safe (catchIO)

import           Seal.Crypto (ProtocolMagic)
import           Seal.Infra.Diffusion.Types (Diffusion)
import           Seal.Infra.Reporting (Reporter (..))
import           Seal.Infra.Reporting.Http (reportNode)
import           Seal.Infra.Reporting.NodeInfo (extendWithNodeInfo)
import           Seal.Infra.Reporting.Wlog (LoggerConfig, withWlogTempFile)
import           Seal.Util.CompileInfo (CompileTimeInfo)
import           Seal.Util.Trace (Severity (Error), Trace, traceWith)

data ProductionReporterParams = ProductionReporterParams
    { prpServers         :: ![Text]
    , prpLoggerConfig    :: !LoggerConfig
    , prpProtocolMagic   :: !ProtocolMagic
    , prpCompileTimeInfo :: !CompileTimeInfo
    , prpTrace           :: !(Trace IO (Severity, Text))
    }

productionReporter
    :: ProductionReporterParams
    -> Diffusion IO -- ^ Used to get status info, not to do any network stuff.
    -> Reporter IO
productionReporter params diffusion = Reporter $ \rt -> withWlogTempFile logConfig $ \mfp -> do
    rt' <- extendWithNodeInfo diffusion rt
    reportNode logTrace protocolMagic compileTimeInfo servers mfp rt'
        `catchIO`
        reportExnHandler rt'
  where
    servers = prpServers params
    logConfig = prpLoggerConfig params
    protocolMagic = prpProtocolMagic params
    compileTimeInfo = prpCompileTimeInfo params
    logTrace = prpTrace params
    --
    reportExnHandler rt e =
        let msgToLog = "reportNode encountered IOException `" <> show e
                    <> "` while trying to report the message:" <> show rt
         in liftIO (traceWith logTrace (Error, msgToLog))
