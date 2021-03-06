-- | Workers for delegation logic.

module Seal.Worker.Delegation
       ( dlgWorkers
       ) where

import           Universum

import           Control.Lens ((%=))
import           Data.Time.Clock (UTCTime, addUTCTime)
import           Data.Time.Units (Second)

import           Seal.Chain.Delegation (HasDlgConfiguration, MonadDelegation,
                     dlgMessageCacheTimeout, dwMessageCache)
import           Seal.Core.Conc (currentTime, delay)
import           Seal.DB.Delegation (DelegationStateAction,
                     runDelegationStateAction)
import           Seal.Infra.Diffusion.Types (Diffusion)
import           Seal.Infra.Reporting (MonadReporting, reportOrLogE)
import           Seal.Infra.Shutdown (HasShutdownContext)
import           Seal.Util (microsecondsToUTC)
import           Seal.Util.LRU (filterLRU)
import           Seal.Util.Wlog (WithLogger)

-- | This is a subset of 'WorkMode'.
type DlgWorkerConstraint ctx m
     = ( MonadIO m
       , MonadDelegation ctx m
       , MonadMask m
       , HasShutdownContext ctx
       , MonadDelegation ctx m
       , WithLogger m
       , MonadReporting m
       , MonadReader ctx m
       , HasDlgConfiguration)


-- | All workers specific to proxy sertificates processing.
dlgWorkers :: (DlgWorkerConstraint ctx m) => [ (Text, Diffusion m -> m ()) ]
dlgWorkers = [ ("delegation worker", \_ -> dlgInvalidateCaches) ]

-- | Runs proxy caches invalidating action every second.
dlgInvalidateCaches :: DlgWorkerConstraint ctx m => m ()
dlgInvalidateCaches =
    -- When dlgInvalidateCaches calls itself directly, it leaks memory. The
    -- reason for that is that reference to dlgInvalidateCaches is kept in
    -- memory (by usage of dlgWorkers) and as it is executed it expands
    -- indefinitely, hence more and more space is needed to store it. Using fix
    -- fixes the problem as it makes dlgInvalidateCaches itself finite in
    -- size. Relevant GHC ticket: https://ghc.haskell.org/trac/ghc/ticket/13080
    fix $ \loop -> do
        -- REPORT:ERROR 'reportOrLogE' in delegation worker.
        invalidate `catchAny` reportOrLogE "Delegation worker, error occurred: "
        delay (1 :: Second)
        loop
  where
    invalidate = do
        curTime <- microsecondsToUTC <$> currentTime
        runDelegationStateAction $ invalidateProxyCaches curTime

-- | Invalidates proxy caches using built-in constants.
invalidateProxyCaches :: HasDlgConfiguration => UTCTime -> DelegationStateAction ()
invalidateProxyCaches curTime =
    dwMessageCache %=
        filterLRU (\t -> addUTCTime (toDiffTime dlgMessageCacheTimeout) t > curTime)
  where
    toDiffTime (t :: Integer) = fromIntegral t
