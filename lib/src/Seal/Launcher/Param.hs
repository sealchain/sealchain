{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Parameters for launching everything.

module Seal.Launcher.Param
       ( LoggingParams (..)
       , BaseParams (..)
       , NodeParams (..)
       ) where

import           Universum

import           Control.Lens (makeLensesWith)

import           Seal.Behavior (BehaviorConfig (..))
import           Seal.Chain.Security (SecurityParams)
import           Seal.Chain.Ssc (SscBehavior)
import           Seal.Chain.Update (UpdateParams)
import           Seal.Core (HasPrimaryKey (..))
import           Seal.Core.NetworkAddress (NetworkAddress)
import           Seal.Crypto (SecretKey)
import           Seal.Infra.DHT.Real.Param (KademliaParams)
import           Seal.Infra.InjectFail (FInjects)
import           Seal.Infra.Network.Types (NetworkConfig)
import           Seal.Infra.Statistics (EkgParams, StatsdParams)
import           Seal.Util.Lens (postfixLFields)
import           Seal.Util.UserPublic (UserPublic)
import           Seal.Util.UserSecret (UserSecret)
import           Seal.Util.Util (HasLens (..))
import           Seal.Util.Wlog (LoggerName)

-- | Contains all parameters required for hierarchical logger initialization.
data LoggingParams = LoggingParams
    { lpDefaultName   :: !LoggerName
    -- ^ Logger name which will be used by default
    , lpHandlerPrefix :: !(Maybe FilePath)
    -- ^ Prefix of path for all logs
    , lpConfigPath    :: !(Maybe FilePath)
    -- ^ Path to logger configuration
    , lpConsoleLog    :: !(Maybe Bool)
    -- ^ Enable console logging (override)
    } deriving (Show)

-- | Contains basic & networking parameters for running node.
data BaseParams = BaseParams
    { bpLoggingParams   :: !LoggingParams  -- ^ Logger parameters
    } deriving (Show)

-- | This data type contains all data necessary to launch node and
-- known in advance (from CLI, configs, etc.)
data NodeParams = NodeParams
    { npDbPathM        :: !(Maybe FilePath)     -- ^ Path to node's database
    , npRebuildDb      :: !Bool                 -- ^ @True@ if data-base should be rebuilt
    , npSecretKey      :: !SecretKey            -- ^ Primary secret key of node
    , npUserPublic     :: !UserPublic           -- ^ All node public keys
    , npUserSecret     :: !UserSecret           -- ^ All node secret keys
    , npBaseParams     :: !BaseParams           -- ^ See 'BaseParams'
    , npJLFile         :: !(Maybe FilePath)     -- ^ File to use for JSON logging.
    , npReportServers  :: ![Text]               -- ^ List of report server URLs
    , npUpdateParams   :: !UpdateParams         -- ^ Params for update system
    , npRoute53Params  :: !(Maybe NetworkAddress) -- ^ Where to listen for the Route53 DNS health-check.
    , npEnableMetrics  :: !Bool                 -- ^ Gather runtime statistics.
    , npEkgParams      :: !(Maybe EkgParams)    -- ^ EKG statistics monitoring.
    , npStatsdParams   :: !(Maybe StatsdParams) -- ^ statsd statistics backend.
    , npNetworkConfig  :: !(NetworkConfig KademliaParams)
    , npBehaviorConfig :: !BehaviorConfig       -- ^ Behavior (e.g. SSC settings)
    , npAssetLockPath  :: !(Maybe FilePath)     -- ^ Path to assetLocked source address file.
    , npFInjects       :: !(FInjects IO)        -- ^ Failure injection handle
    } -- deriving (Show)

makeLensesWith postfixLFields ''NodeParams
makeLensesWith postfixLFields ''BehaviorConfig

instance HasLens UpdateParams NodeParams UpdateParams where
    lensOf = npUpdateParams_L

instance HasLens BehaviorConfig NodeParams BehaviorConfig where
    lensOf = npBehaviorConfig_L
instance HasLens SecurityParams NodeParams SecurityParams where
    lensOf = npBehaviorConfig_L . bcSecurityParams_L
instance HasLens SscBehavior NodeParams SscBehavior where
    lensOf = npBehaviorConfig_L . bcSscBehavior_L

instance HasLens (NetworkConfig KademliaParams) NodeParams (NetworkConfig KademliaParams) where
    lensOf = npNetworkConfig_L

instance HasPrimaryKey NodeParams where
    primaryKey = npSecretKey_L
