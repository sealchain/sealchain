-- | Higher-level functionality of LRC DB.

module Seal.DB.Lrc.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Seal.Chain.Genesis as Genesis (Config (..),
                     configBlockVersionData, configFtsSeed)
import           Seal.DB.Class (MonadDB)
import           Seal.DB.Error (DBError (..))
import           Seal.DB.Lrc.Common (prepareLrcCommon)
import           Seal.DB.Lrc.Issuers (prepareLrcIssuers)
import           Seal.DB.Lrc.Leaders (prepareLrcLeaders)
import           Seal.DB.Lrc.Richmen (prepareLrcRichmen, tryGetUSRichmen)
import           Seal.DB.Lrc.Seed (prepareLrcSeed)

import           Seal.Util (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB :: MonadDB m => Genesis.Config -> m ()
prepareLrcDB genesisConfig = do
    prepareLrcLeaders genesisConfig
    prepareLrcRichmen genesisConfig
    let cantReadErr =
            DBMalformed "Can't read richmen US after richmen initialization"
    totalStake <-
        fst
            <$> (   maybeThrow cantReadErr
                =<< tryGetUSRichmen (configBlockVersionData genesisConfig) 0
                )
    prepareLrcIssuers totalStake
    prepareLrcSeed (configFtsSeed genesisConfig)
    prepareLrcCommon
