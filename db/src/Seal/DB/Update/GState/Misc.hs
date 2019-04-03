-- | Interface for the Misc DB

module Seal.DB.Update.GState.Misc
       ( isUpdateInstalled
       , affirmUpdateInstalled
       ) where

import           Universum

import           Formatting (sformat)

import           Seal.Binary.Class (Raw)
import           Seal.Crypto (Hash, hashHexF)
import           Seal.DB.Class (MonadDB)
import           Seal.DB.Misc.Common (miscGetBi, miscPutBi)

isUpdateInstalled :: MonadDB m => Hash Raw -> m Bool
isUpdateInstalled h = isJust <$> miscGetBi @() (updateTrackKey h)

affirmUpdateInstalled :: MonadDB m => Hash Raw -> m ()
affirmUpdateInstalled h = miscPutBi (updateTrackKey h) ()

updateTrackKey :: Hash Raw -> ByteString
updateTrackKey h = "updinst/" <> encodeUtf8 (sformat hashHexF h)
