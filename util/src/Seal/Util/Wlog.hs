{-# LANGUAGE Rank2Types #-}

-- | a compatible interface to 'log-warper'
--   logging output is directed to 'katip'

module Seal.Util.Wlog
        ( -- * CanLog
          CanLog (..)
        , WithLogger
          -- * Pure logging
        , dispatchEvents
        , LogEvent (..)
        , NamedPureLogger (..)
        , launchNamedPureLog
        , runNamedPureLog
          -- * Setup
        , setupLogging
        , setupLogging'
        , setupTestLogging
          -- * Logging functions
        , logDebug
        , logError
        , logInfo
        , logNotice
        , logWarning
        , logMessage
          -- * LoggerName
        , LoggerName
        , LoggerNameBox (..)
        , HasLoggerName (..)
        , usingLoggerName
        , addLoggerName
        , setLoggerName
          -- * LoggerConfig
        , LoggerConfig (..)
        , lcTree
        , parseLoggerConfig
          -- * Builders for 'LoggerConfig'
        , productionB
          -- * Severity
        , Severity (..)
          -- * Saving Changes
        , retrieveLogContent
          -- * Logging messages with a condition
        , logMCond
          -- * Utility functions
        , removeAllHandlers
        , centiUtcTimeF
        , setLogPrefix
        , getLinesLogged
        ) where

import           Seal.Util.Log (LoggerName, Severity (..))
import           Seal.Util.Log.LoggerConfig (LoggerConfig (..), lcTree,
                     parseLoggerConfig, setLogPrefix)
import           Seal.Util.Wlog.Compatibility (CanLog (..), HasLoggerName (..),
                     LogEvent (..), LoggerNameBox (..), NamedPureLogger (..),
                     WithLogger, addLoggerName, centiUtcTimeF, dispatchEvents,
                     getLinesLogged, launchNamedPureLog, logDebug, logError,
                     logInfo, logMCond, logMessage, logNotice, logWarning,
                     productionB, removeAllHandlers, retrieveLogContent,
                     runNamedPureLog, setLoggerName, setupLogging,
                     setupLogging', setupTestLogging, usingLoggerName)

