{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Seal.Contract.Native
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Seal.Contract builtins/standard library.
--

module Seal.Chain.Contract.Native
    (natives
    ,nativeDefs
    ,moduleToMap
    ,lengthDef
    ,enforceDef
    ,enforceOneDef
    ,pactVersionDef
    ,formatDef
    ,hashDef
    ,ifDef
    ,readDecimalDef
    ) where

import Control.Monad
import Prelude
import qualified Data.HashMap.Strict as M



import Seal.Contract.Native.Db
import Seal.Contract.Native.Internal
import Seal.Contract.Native.Time
import Seal.Contract.Native.Ops
import Seal.Contract.Native.Keysets
import Seal.Contract.Types.Runtime
import Seal.Core.Common (decodeTextAddress)
import Seal.Chain.Contract.Classes(accountRedeem,accountDeposit)
import Seal.Core.Common (CoinGroup(..),Currency (..),Account(..))
import Seal.Contract.Native hiding (natives,nativeDefs)


-- | All production native modules.
natives :: [NativeModule]
natives = [
  langDefs,
  dbDefs,
  timeDefs,
  opDefs,
  sealDefs,
  keyDefs]


-- | Production native modules as a dispatch map.
nativeDefs :: M.HashMap Name Ref
nativeDefs = mconcat $ map moduleToMap natives


sealDefs :: NativeModule
sealDefs =
    ("seal",[
     defRNative "gd-send" gdSend (funType a [("fromAccount",tTyString),("toAccount",tTyString),("amount",tTyInteger)])
     "send GoldDollar,eg:gd-send(fromAccount, toAccount, amount)"
     ,defRNative "seal-send" sealSend (funType a [("fromAccount",tTyString),("toAccount",tTyString),("amount",tTyInteger)])
     "send Seal Coin,eg:seal-send(fromAccount, toAccount, amount)" 
    ])
  
a :: Type n
a = mkTyVar "a" []

lookupObj' :: (Eq n, Show n) => String -> [(Term n, Term n)] -> Eval m (Term n)
lookupObj' s = lookupObj $ toTerm s

--稳定币转账
gdSend :: RNativeFun e
gdSend i [TObject ls _ _] = do
  fromAccount <- lookupObj' ":from" ls
  toAccount <- lookupObj' ":to" ls
  amount <- lookupObj' ":amount" ls
  gdSend i [fromAccount,toAccount,amount]
gdSend i [faddr@(TLitString fromAccount),TLitString toAccount,TLitInteger amount] = do
  --A 账号扣钱 B 账号加钱
  fromAddr <- case decodeTextAddress fromAccount of
                Left t -> evalError' i $ "fromAccount:" ++ show fromAccount ++  " decode failed" ++ show t
                Right addr -> return addr
  toAddr <- case decodeTextAddress toAccount of
                Left t -> evalError' i $ "toAccount:" ++ show toAccount ++ " decode failed" ++ show t
                Right addr -> return addr
  _ <- enforceKey' i [faddr]
  let inCoins = CoinGroup (mkMoney 0) (mkMoney 0) (mkMoney $ fromIntegral amount)
  -- 转出
  accountRedeem (Account fromAddr) inCoins
  -- 转入
  accountDeposit (Account toAddr) inCoins
  return $ toTerm $ True
gdSend i as = argsError i as

--Seal币转账
sealSend :: RNativeFun e
sealSend i [TObject ls _ _] = do
  fromAccount <- lookupObj' ":from" ls
  toAccount <- lookupObj' ":to" ls
  amount <- lookupObj' ":amount" ls
  sealSend i [fromAccount,toAccount,amount]
sealSend i [faddr@(TLitString fromAccount),TLitString toAccount,TLitInteger amount] = do
  --A 账号扣钱 B 账号加钱
  fromAddr <- case decodeTextAddress fromAccount of
                Left t -> evalError' i $ "fromAccount:" ++ show fromAccount ++  " decode failed" ++ show t
                Right addr -> return addr
  toAddr <- case decodeTextAddress toAccount of
                Left t -> evalError' i $ "toAccount:" ++ show toAccount ++ " decode failed" ++ show t
                Right addr -> return addr
  _ <- enforceKey' i [faddr]
  let inCoins = CoinGroup (mkMoney $ fromIntegral amount) (mkMoney 0) (mkMoney 0) 
  -- 转出
  accountRedeem (Account fromAddr) inCoins
  -- 转入
  accountDeposit (Account toAddr) inCoins
  return $ toTerm $ True
sealSend i as = argsError i as
