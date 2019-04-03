{-# LANGUAGE TemplateHaskell #-}
module Seal.Chain.Contract.Types
  ( ReplState(..),rEnv,rEvalState,rOut,rFile,rName
  , TestResult(..)
  , Repl
  , LibOp(..),Hdl(..)
  , LibState(..),rlsPure,rlsOp,rlsTxName,rlsTests
  , Tx(..)
  ) where

import Control.Lens (makeLenses)
import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)
import Seal.Contract.PersistPactDb (DbEnv)
import Seal.Contract.Persist.MPTree (MPtreeDb(..))

import Seal.Contract.Types.Runtime (EvalEnv,EvalState,Term,Name,FunApp,Info,Hash,ModuleName)
import Data.Text (Text)
import Prelude

data Hdl = HOut|HErr


data TestResult = TestResult
  { trName :: Text
  , trFailure :: Maybe (FunApp,Text)
  }

data ReplState = ReplState {
      _rEnv :: EvalEnv LibState
    , _rEvalState :: EvalState
    -- , _rMode :: ReplMode
    , _rOut :: String
    , _rFile :: Maybe FilePath
    , _rName :: Maybe (ModuleName,Hash)
    }

type Repl a = StateT ReplState IO a


data LibOp =
    Noop |
    UpdateEnv (Endo (EvalEnv LibState)) |
    Load FilePath Bool |
    Tx Info Tx (Maybe Text) |
    Print (Term Name) |
    TcErrors [String]
instance Default LibOp where def = Noop

data Tx = Begin|Commit|Rollback deriving (Eq,Show,Bounded,Enum,Ord)

data LibState = LibState {
      _rlsPure :: MVar (DbEnv MPtreeDb)
    , _rlsOp :: LibOp
    , _rlsTxName :: Maybe Text
    , _rlsTests :: [TestResult]
}


makeLenses ''LibState
makeLenses ''ReplState
