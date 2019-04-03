-- | DB operations for storing and dumping SscGlobalState.

module Seal.DB.Ssc.GState
       ( getSscGlobalState
       , sscGlobalStateToBatch
       , initSscDB
       ) where

import           Universum

import           Data.Default (def)
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Seal.Binary.Class (serialize')
import           Seal.Chain.Ssc (SscGlobalState (..), VssCertificatesMap)
import qualified Seal.Chain.Ssc as VCD
import           Seal.DB (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Seal.DB.Error (DBError (DBMalformed))
import           Seal.DB.GState.Common (gsGetBi, gsPutBi)
import           Seal.Util.Util (maybeThrow)

getSscGlobalState :: (MonadDBRead m) => m SscGlobalState
getSscGlobalState =
    maybeThrow (DBMalformed "SSC global state DB is not initialized") =<<
    gsGetBi sscKey

sscGlobalStateToBatch :: SscGlobalState -> SscOp
sscGlobalStateToBatch = PutGlobalState

initSscDB :: (MonadDB m) => VssCertificatesMap -> m ()
initSscDB genesisVssCerts = gsPutBi sscKey (def {_sgsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ genesisVssCerts

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data SscOp
    = PutGlobalState !SscGlobalState

instance Buildable SscOp where
    build (PutGlobalState gs) = bprint ("SscOp ("%build%")") gs

instance RocksBatchOp SscOp where
    toBatchOp (PutGlobalState gs) = [Rocks.Put sscKey (serialize' gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

sscKey :: ByteString
sscKey = "ssc/"
