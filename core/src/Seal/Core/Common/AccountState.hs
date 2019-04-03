{-# LANGUAGE RecordWildCards #-}

module Seal.Core.Common.AccountState
( AccountState (..)
) where

import           Universum

import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable as Buildable

import           Seal.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Seal.Core.Common.CoinGroup

data AccountState 
    = AccountState 
    { nonce    :: !Word64
    , balance  :: !CoinGroup
    } deriving (Show, Eq, Ord)

instance Buildable AccountState where
    build AccountState{..} =
        bprint ("balance = "%build%", nonce = "%int) balance nonce

instance Bi AccountState where
    encode AccountState{..} = encodeListLen 2 <> encode nonce <> encode balance
                                        
    decode = do
        enforceSize "AccountState" 2
        nonce <- decode
        balance <- decode
        return $ AccountState{..}
