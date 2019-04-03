module Seal.Infra.Shutdown.Logic
       ( triggerShutdown
       , waitForShutdown
       ) where

import           Universum

import           Control.Concurrent.STM (check, readTVar, writeTVar)

import           Seal.Infra.InjectFail (FInject (..), testLogFInject)
import           Seal.Infra.Shutdown.Class (HasShutdownContext (..))
import           Seal.Infra.Shutdown.Types (ShutdownContext (..), shdnFInjects,
                     shdnIsTriggered)
import           Seal.Util.Wlog (WithLogger, logInfo)

triggerShutdown
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => m ()
triggerShutdown = do
    shutCtx <- view shutdownContext
    doFail <- liftIO $ testLogFInject (shutCtx ^. shdnFInjects) FInjIgnoreShutdown
    unless doFail $ do
      logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
      view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ()
waitForShutdown (ShutdownContext shutdownTriggered _) = atomically (readTVar shutdownTriggered >>= check)
