{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Reporting functionality abstracted.
--
-- FIXME this is only about "misbehaviour metrics". Rename it.

module Seal.Core.Reporting.MemState
       ( MisbehaviorMetrics (..)
       , HasMisbehaviorMetrics (..)
       , initializeMisbehaviorMetrics
       , mmRollbacks
       , mmSscFailures
       , mmIgnoredCommitments
       ) where

import           Control.Lens (makeLenses)
import qualified System.Metrics as Metrics
import           System.Metrics.Counter (Counter)
import           System.Metrics.Gauge (Gauge)
import           Universum

import           Seal.Core.Metrics.Constants (withSealNamespace)

-- | EKG metric values for misbehaviors
data MisbehaviorMetrics = MisbehaviorMetrics
    { _mmRollbacks          :: Gauge
    -- ^ Amount of rolled back blocks after latest fork
    , _mmSscFailures        :: Counter
    -- ^ Amount of SSC computation failures
    , _mmIgnoredCommitments :: Gauge
    -- ^ Number of epochs in a row without expected node's commitments
    }

makeLenses ''MisbehaviorMetrics

class HasMisbehaviorMetrics ctx where
    misbehaviorMetrics :: Lens' ctx (Maybe MisbehaviorMetrics)

initializeMisbehaviorMetrics :: MonadIO m => Metrics.Store -> m MisbehaviorMetrics
initializeMisbehaviorMetrics store = liftIO $ do
    _mmRollbacks <- Metrics.createGauge (withSealNamespace "BlockRollbacks") store
    _mmSscFailures <- Metrics.createCounter (withSealNamespace "SCCComputationFailures") store
    _mmIgnoredCommitments <- Metrics.createGauge (withSealNamespace "IgnoredCommitments") store
    return MisbehaviorMetrics{..}
