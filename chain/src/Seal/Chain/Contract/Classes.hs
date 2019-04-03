{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}



module Seal.Chain.Contract.Classes where

import           Universum hiding (Lens',view,use)
import           Seal.Contract.Persist.MPTree
import qualified Seal.Util.Modifier as MM
import           Seal.Mpt.MerklePatricia (MPDB(..),getKeyVal)
import           Seal.Mpt.MerklePatricia.Utils(bytesToNibbleString)
import           Control.Lens
import           Seal.Contract.Types.Runtime (PactError(..),Eval(..),eeAccountMPDB,evalAccountMM)
import           Seal.Binary.Class (decodeFull', serialize')
-- import           Seal.Chain.Txp.Toil.Failure (ToilVerFailure (..))
import           Seal.Core (unsafeAddCoinGroup,  unsafeSubCoinGroup, anyGreatThanCoinGroup, AccountState(..))
import           Seal.Core.Common (Account, AccountModifier, CoinGroup(..))


type PactModifier = MPtreeDb

class Monad m => AccountSupport m where
  lookupAccountState :: Account -> m (Maybe AccountState)
  insertAccountState :: Account -> AccountState -> m ()

-- account state root
class MonadIO m => HasAccountMPDB m where
  getAccountMPDB :: m MPDB

-- account map modifier 
class HasAccountMM m where
  getAccountMM :: m AccountModifier 
  setAccountMM :: AccountModifier -> m ()

data InvalidAccount = ToilAccountInputInvalid
    deriving (Show, Eq)

instance Exception InvalidAccount



accountRedeem :: (MonadIO m,AccountSupport m,MonadThrow m) => Account -> CoinGroup -> m ()
accountRedeem acc inCoins = do
  -- let inCoins = CoinGroup (mkMoney 0) (mkMoney 0) (mkMoney $ fromIntegral amount)
  mstate <- lookupAccountState acc
  case mstate of
    Just st@AccountState{..} -> do
        liftIO $ putStrLn $ "gdsend inCoins " ++ show inCoins
        liftIO $ putStrLn $ "gdsend coins " ++ show balance
        when (inCoins `anyGreatThanCoinGroup` balance) $
               throwM ToilAccountInputInvalid
        let newst = st{ balance=unsafeSubCoinGroup balance inCoins
                      -- , nonce=nonce+1
                      }
        insertAccountState acc newst
    Nothing -> throwM ToilAccountInputInvalid

accountDeposit :: (AccountSupport m) => Account -> CoinGroup -> m ()
accountDeposit acc inCoins = do
  -- let inCoins = CoinGroup (mkMoney 0) (mkMoney 0) (mkMoney $ fromIntegral amount)
  mstate <- lookupAccountState acc
  let newst = case mstate of
                Just st@AccountState{balance=balance} ->
                    st{balance=unsafeAddCoinGroup balance inCoins}
                Nothing -> AccountState 0 inCoins
  insertAccountState acc newst


instance (Monad m, HasAccountMPDB m, HasAccountMM m) => AccountSupport m where
  lookupAccountState acc = do
    mpdb <- getAccountMPDB
    mm <- getAccountMM
    let accToKey = bytesToNibbleString . serialize'
    let lookupAcc acct = do
          mvalue <- getKeyVal mpdb (accToKey acct)
          return $ mvalue >>= either (const Nothing) Just . decodeFull'
    MM.lookupM lookupAcc acc mm
  
  insertAccountState acc accst = do
    mm <- getAccountMM
    setAccountMM $ MM.insert acc accst mm

-- type AccountMapModifier = MM.MapModifier Account AccountState

data ApplyEnv = ApplyEnv {
    _aeAccountMPDB :: MPDB
  , _aePactMPDB    :: MPDB
}
makeLenses ''ApplyEnv

data ApplyState = ApplyState {
    _asAcctModifier  :: AccountModifier
  , _asPactModifier  :: PactModifier
}
makeLenses ''ApplyState

type ApplyM m = StateT ApplyState (ReaderT ApplyEnv m)

instance MonadIO m => HasAccountMPDB (ApplyM m)  where
  getAccountMPDB = view aeAccountMPDB

instance Monad m=> HasAccountMM (ApplyM m) where
  getAccountMM = use asAcctModifier
  setAccountMM = assign asAcctModifier


instance HasAccountMPDB (Eval e)  where
  getAccountMPDB = view eeAccountMPDB

instance HasAccountMM (Eval e) where
  getAccountMM = use evalAccountMM
  setAccountMM = assign evalAccountMM

-- 合约执行错误后返回错误信息
runApply :: ApplyState -> ApplyEnv -> ApplyM IO a -> IO (Either String a, ApplyState)
runApply aState env act = runReaderT (runStateT act' aState ) env
  where
    act' = catch (Right <$> act) (\e -> return . Left $ show (e::PactError))
      


