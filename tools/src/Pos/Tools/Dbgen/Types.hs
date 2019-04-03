{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}

module Seal.Tools.Dbgen.Types where

import           Universum

import           Data.Map (Map)
import           Seal.Wallet.Web.Mode (MonadWalletWebMode, WalletWebMode)

type UberMonad a = forall ctx. MonadWalletWebMode ctx WalletWebMode => WalletWebMode a

data Method = GetWallets
    deriving (Show, Read)

type Methods = MonadWalletWebMode () WalletWebMode => Map Method (WalletWebMode ())
