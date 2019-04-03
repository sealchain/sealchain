{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

{- |

The initialization mode is used to build the node context (node resources).
We need a separate mode for this because our initialization procedures require
database access, slotting, logging, etc.

The tricky part specific to this mode is the use of futures. Some parts of the
'InitModeContext' become available *during* initialization, so we create thunks
out of thin air and fill them as we go. This way 'InitMode' has all instances
it needs at all stages of initialization, but some of those instances can be
unusable until the relevant parts of the context are set.

-}

module Seal.Launcher.Mode
       ( InitMode
       , runInitMode
       , InitModeContext(..)
       ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl

import           Seal.Core (Timestamp)
import           Seal.Core.Slotting (MonadSlotsData)
import           Seal.DB (NodeDBs)
import           Seal.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault)
import           Seal.DB.Class (MonadDB (..), MonadDBRead (..))
import           Seal.DB.Lrc (LrcContext)
import           Seal.DB.Rocks (dbDeleteDefault, dbGetDefault,
                     dbIterSourceDefault, dbPutDefault, dbWriteBatchDefault)
import           Seal.Infra.Slotting (HasSlottingVar (..))
import           Seal.Infra.Slotting.Class (MonadSlots (..))
import           Seal.Infra.Slotting.Impl (SimpleSlottingStateVar,
                     currentTimeSlottingSimple, getCurrentSlotBlockingSimple,
                     getCurrentSlotInaccurateSimple, getCurrentSlotSimple)
import           Seal.Infra.Slotting.Types (SlottingData)
import           Seal.Util.Lens (postfixLFields)
import           Seal.Util.Util (HasLens (..))

-- The fields are lazy on purpose: this allows using them with
-- futures.
data InitModeContext = InitModeContext
    { imcNodeDBs          :: NodeDBs
    , imcSlottingVar      :: (Timestamp, TVar SlottingData)
    , imcSlottingStateVar :: SimpleSlottingStateVar
    , imcLrcContext       :: LrcContext
    }

makeLensesWith postfixLFields ''InitModeContext

type InitMode = Mtl.ReaderT InitModeContext IO

runInitMode :: InitModeContext -> InitMode a -> IO a
runInitMode = flip Mtl.runReaderT

instance HasLens NodeDBs InitModeContext NodeDBs where
    lensOf = imcNodeDBs_L

instance HasLens SimpleSlottingStateVar InitModeContext SimpleSlottingStateVar where
    lensOf = imcSlottingStateVar_L

instance HasLens LrcContext InitModeContext LrcContext where
    lensOf = imcLrcContext_L

instance HasSlottingVar InitModeContext where
    slottingTimestamp = imcSlottingVar_L . _1
    slottingVar = imcSlottingVar_L . _2

instance MonadDBRead InitMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

instance MonadDB InitMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

instance MonadSlotsData ctx InitMode => MonadSlots ctx InitMode where
    getCurrentSlot           = getCurrentSlotSimple
    getCurrentSlotBlocking   = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting      = currentTimeSlottingSimple
