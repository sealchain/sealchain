{-# LANGUAGE RecordWildCards #-}

-- | Getter params from Args

module Seal.Client.CLI.Params
       ( loggingParams
       , getBaseParams
       , getKeyfilePath
       , getNodeParams
       , gtSscParams
       ) where

import           Universum

import           Data.Default (def)
import qualified Data.Yaml as Yaml

import           Seal.Behavior (BehaviorConfig (..))
import           Seal.Chain.Genesis (GeneratedSecrets)
import           Seal.Chain.Ssc (SscParams (..))
import           Seal.Chain.Update (UpdateParams (..))
import           Seal.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Seal.Client.CLI.Options (CommonArgs (..))
import           Seal.Client.CLI.Secrets (prepareUserSecret)
import           Seal.Crypto (VssKeyPair)
import           Seal.Infra.InjectFail (mkFInjects)
import           Seal.Infra.Network.CLI (intNetworkConfigOpts)
import           Seal.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Seal.Util.UserPublic (peekUserPublic)
import           Seal.Util.UserSecret (peekUserSecret, usVss)
import           Seal.Util.Util (eitherToThrow)
import           Seal.Util.Wlog (LoggerName, WithLogger)

loggingParams :: LoggerName -> CommonNodeArgs -> LoggingParams
loggingParams defaultName CommonNodeArgs{..} =
    LoggingParams
    { lpHandlerPrefix = logPrefix commonArgs
    , lpConfigPath    = logConfig commonArgs
    , lpDefaultName   = defaultName
    , lpConsoleLog    = Just (not $ logConsoleOff commonArgs)
    }

getBaseParams :: LoggerName -> CommonNodeArgs -> BaseParams
getBaseParams defaultLoggerName args@CommonNodeArgs {..} =
    BaseParams { bpLoggingParams = loggingParams defaultLoggerName args }

gtSscParams :: CommonNodeArgs -> VssKeyPair -> BehaviorConfig -> SscParams
gtSscParams CommonNodeArgs {..} vssSK BehaviorConfig{..} =
    SscParams
    { spSscEnabled = True
    , spVssKeyPair = vssSK
    , spBehavior   = bcSscBehavior
    }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..}
    = case devGenesisSecretI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath

getNodeParams ::
       ( MonadIO m
       , WithLogger m
       , MonadCatch m
       )
    => LoggerName
    -> CommonNodeArgs
    -> NodeArgs
    -> Maybe GeneratedSecrets
    -> m (NodeParams, Maybe SscParams)
getNodeParams defaultLoggerName cArgs@CommonNodeArgs{..} NodeArgs{..} mGeneratedSecrets = do
    (primarySK, userSecret) <- prepareUserSecret cArgs mGeneratedSecrets
        =<< peekUserSecret (getKeyfilePath cArgs)
    userPublic <- peekUserPublic publicKeyfilePath
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> eitherToThrow =<< liftIO (Yaml.decodeFileEither fp)
    npFInjects <- liftIO $ mkFInjects cnaFInjectsSpec

    let nodeParams = NodeParams
            { npDbPathM = dbPath
            , npRebuildDb = rebuildDB
            , npSecretKey = primarySK
            , npUserSecret = userSecret
            , npUserPublic = userPublic
            , npBaseParams = getBaseParams defaultLoggerName cArgs
            , npJLFile = jlPath
            , npReportServers = reportServers commonArgs
            , npUpdateParams = UpdateParams
                { upUpdatePath    = updateLatestPath
                , upUpdateWithPkg = updateWithPackage
                , upUpdateServers = updateServers commonArgs
                }
            , npRoute53Params = route53Params
            , npEnableMetrics = enableMetrics
            , npEkgParams = ekgParams
            , npStatsdParams = statsdParams
            , npAssetLockPath = cnaAssetLockPath
            , ..
            }

    let sscParams = gtSscParams cArgs
           <$> (userSecret ^. usVss)
           <*> pure npBehaviorConfig

    return (nodeParams, sscParams)
