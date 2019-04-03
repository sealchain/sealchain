-- | Server part.

module Seal.Communication.Server
       ( serverLoggerName
       ) where

import           Seal.Util.Wlog (LoggerName)

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
