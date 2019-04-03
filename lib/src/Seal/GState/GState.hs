-- | Higher-level functions working with GState DB.

module Seal.GState.GState
       ( prepareGStateDB
       ) where

import           Universum

import           Seal.Chain.Block (HeaderHash)
import           Seal.Chain.Genesis as Genesis (Config (..),
                     configHeavyDelegation, configVssCerts)
import           Seal.DB.Block (initGStateBlockExtra)
import           Seal.DB.Class (MonadDB)
import           Seal.DB.Delegation (initGStateDlg)
import           Seal.DB.GState.Common (initGStateCommon, isInitialized,
                     setInitialized)
import           Seal.DB.Ssc (initSscDB)
import           Seal.DB.Txp (initGStateStakes, initGStateUtxo)
import           Seal.DB.Update (initGStateUS)

-- | Put missing initial data into GState DB.
prepareGStateDB ::
       forall ctx m.
       ( MonadReader ctx m
       , MonadDB m
       )
    => Genesis.Config
    -> HeaderHash
    -> m ()
prepareGStateDB genesisConfig initialTip = unlessM isInitialized $ do
    initGStateCommon initialTip
    initGStateUtxo genesisData
    initSscDB $ configVssCerts genesisConfig
    initGStateStakes genesisData
    initGStateUS genesisConfig
    initGStateDlg $ configHeavyDelegation genesisConfig
    initGStateBlockExtra (configGenesisHash genesisConfig) initialTip

    setInitialized
  where genesisData = configGenesisData genesisConfig

-- The following is not used in the project yet. To be added back at a
-- later stage when needed.

{-
usingGStateSnapshot :: (MonadRealDB ctx m, MonadMask m) => m a -> m a
usingGStateSnapshot action = do
    db <- _gStateDB <$> getNodeDBs
    let readOpts = rocksReadOpts db
    usingSnapshot db (\(Snapshot sn) ->
        usingReadOptions readOpts {Rocks.useSnapshot = Just sn} gStateDB action)
-}
