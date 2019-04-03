{-# LANGUAGE RecordWildCards #-}

-- | Types used for block processing: most importantly, 'Undo' and 'Blund'.

module Seal.Chain.Block.Types
       ( SlogUndo (..)
       , Undo (..)
       , buildUndo
       , Blund

       , LastKnownHeader
       , LastKnownHeaderTag
       , MonadLastKnownHeader
       ) where

import           Universum

import           Formatting (Format, bprint, build, later, (%))
import           Serokell.Util.Text (listJson)

import           Seal.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Seal.Chain.Block.Block (Block)
import           Seal.Chain.Block.Header (BlockHeader, HasHeaderHash (..))
import           Seal.Chain.Block.Slog.Types (SlogUndo (..), buildSlogUndo)
import           Seal.Chain.Delegation (DlgUndo)
import           Seal.Chain.Txp (TxpUndo)
import           Seal.Chain.Update (USUndo)
import           Seal.Core (HasDifficulty (..), SlotCount)
import           Seal.Util.Util (HasLens (..))

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx   :: !TxpUndo
    , undoDlg  :: !DlgUndo
    , undoUS   :: !USUndo
    , undoSlog :: !SlogUndo
    } deriving (Eq, Show, Generic)

instance NFData Undo

-- | Block and its Undo.
type Blund = (Block, Undo)

buildUndo :: SlotCount -> Format r (Undo -> r)
buildUndo epochSlots = later $ \Undo{..} ->
    bprint ("Undo:\n"%
            "  undoTx: "%listJson%"\n"%
            "  undoDlg: "%build%"\n"%
            "  undoUS: "%build%"\n"%
            "  undoSlog: "%buildSlogUndo epochSlots)
            (map (bprint listJson) undoTx) undoDlg undoUS undoSlog

instance HasDifficulty Blund where
    difficultyL = _1 . difficultyL

instance HasHeaderHash Blund where
    headerHash = headerHash . fst

-- | For a description of what these types mean,
-- please refer to @NodeContext@ in @Seal.Context.Context@.
data LastKnownHeaderTag
type LastKnownHeader = TVar (Maybe BlockHeader)
type MonadLastKnownHeader ctx m
     = (MonadReader ctx m, HasLens LastKnownHeaderTag ctx LastKnownHeader)

-- TH derived instances at the end of the file.

deriveSimpleBi ''Undo [
    Cons 'Undo [
        Field [| undoTx    :: TxpUndo  |],
        Field [| undoDlg   :: DlgUndo  |],
        Field [| undoUS    :: USUndo   |],
        Field [| undoSlog  :: SlogUndo |]
    ]]
