{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Seal.Contract.Repl
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- REPL/interactive interpreter for Seal.Contract. Includes
-- "Repl lib" automatically for non-blockchain interactive
-- functionality.
--

module Seal.Chain.Contract.Repl where

import Control.Applicative
import Control.Lens hiding (op)
import Control.Monad.Catch
import Control.Monad.State.Strict
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.UTF8 as BS
import Data.Char
import Data.Default
import Data.List
import qualified Data.HashMap.Strict as HM
import Prelude hiding (exp)
import Text.Trifecta as TF hiding (line,err,try,newline)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Word (Word8)
import Text.Trifecta.Delta
import Control.Concurrent
import Data.Monoid (appEndo)
import System.FilePath
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BSC
import System.IO
import Bound (abstract)


import Seal.Contract.Compile
import Seal.Contract.Parse
import Seal.Contract.Eval
import Seal.Contract.Types.Runtime hiding (TxId(..))
import Seal.Chain.Contract.Native 
import Seal.Contract.Types.Logger
import Seal.Chain.Contract.Types
import Seal.Chain.Contract.Lib
import Seal.Contract.Gas
import Universum ((<>))
import Seal.Core.Common
import Seal.Chain.Contract.Classes
import Seal.Contract.Persist.MPTree (MPtreeDb(..),commitMpdb)
import Seal.Core.Common(addrToBase58)
import Seal.Contract.PersistPactDb
import Seal.Mpt.MerklePatricia
import Seal.Chain.Contract.Builtin (tokenSrc,tokenResult)
import Seal.Chain.Txp.Tx (TxId)
import Seal.Chain.Txp.Toil.Types (AccountModifier)


-- command执行入口, 一个SealTx调用一次
applyCommands :: MonadIO m => TxId -> [String] -> [Account] -> ApplyM m ()
applyCommands txid cmds sigs = do
  pactMM <- use asPactModifier
  acoutMM <- use asAcctModifier
  accountMpdb <- view aeAccountMPDB
  initRs <- initReplState txid pactMM accountMpdb acoutMM
  let s = setReplLib initRs --add replDefs
  (acMM,pactMpdb,value) <- liftIO $ do
    (v,s') <- evalString s sigs $ unlines cmds
    lbs <- takeMVar $ view (rEnv . eePactDbVar) s'
    rls <- takeMVar $ view rlsPure lbs
    let newMM = view (rEvalState . evalAccountMM) s'
    let newMpdb = view db rls
    return (newMM,newMpdb,v)
  asPactModifier .= pactMpdb
  asAcctModifier .= acMM
  liftIO $ print $ "tx result:" ++ show value
  return ()

commitPactModify :: PactModifier -> IO MPDB
commitPactModify pactMM = do
  mptree <- commitMpdb pactMM
  return $ _rootMPDB mptree 


endsWith :: Eq a => [a] -> [a] -> Bool
endsWith v s = s == reverse (take (length s) (reverse v))

initReplState :: MonadIO m => TxId -> PactModifier -> MPDB -> AccountModifier -> m ReplState
initReplState tId mpdb acMPDB acMM = do
  let evelState = EvalState def def def 0 acMM
  evelEnv <- liftIO $ initPureEvalEnv tId mpdb acMPDB
  return (ReplState evelEnv evelState def def Nothing)
--SealTxId 作用?
initPureEvalEnv :: TxId -> PactModifier -> MPDB -> IO (EvalEnv LibState)
initPureEvalEnv sealTxId mpdb acMPDB = do
  mv <- initLibState neverLog mpdb >>= newMVar
  let strTx = show sealTxId
  let txHash = Hash $ BSC.pack strTx
  return $ EvalEnv (RefStore nativeDefs mempty) def Null (Just 0) def def mv repldb def txHash freeGasEnv acMPDB

account2PublicKey :: Account -> PublicKey
account2PublicKey account = PublicKey $ addrToBase58 (getAccount account)

errToUnit :: Functor f => f (Either e a) -> f (Either () a)
errToUnit a = either (const (Left ())) Right <$> a

toUTF8Bytes :: String -> [Word8]
toUTF8Bytes = BS.unpack . encodeUtf8 . Text.pack

utf8BytesLength :: String -> Int
utf8BytesLength = length . toUTF8Bytes

handleParse :: TF.Result [Exp Parsed] -> ([Exp Parsed] -> Repl (Either String a)) -> Repl (Either String a)
handleParse (TF.Failure e) _ = do
  -- mode <- use rMode
  outStrLn HErr (renderPrettyString (RPlain) (_errDoc e))
  return (Left (renderCompactString $ _errDoc e))
handleParse (TF.Success es) a = a es

parsedCompileEval :: String -> TF.Result [Exp Parsed] -> Repl (Either String (Term Name))
parsedCompileEval src r = do
  let exps = getExpParsed r
  rs <- forM exps $ \ex -> handleCompile src ex
  -- 找出TModule里面的[Term]
  t1 <- forM rs $ \ex -> do
          case ex of
            Right t -> do
              case t of 
                TDef {} -> return [t]
                TConst {} -> return [t]
                TSchema {} -> return [t]
                TTable {} -> return [t]
                TEvent {} -> return [t]
                TImplements {} -> return [t]
                _         -> return []
            Left _ -> return []
  let t3 = concat t1
  -- 找出Module里面各个Term的Code，后面需要将这些code组合成行的code，存入Module中
  tcode <- forM t3 $ \te -> do
    let inf = _tInfo te
    case inf of 
      Info Nothing -> return []
      Info (Just ((Code c),_)) -> return [c]
  let codes = Text.concat $ concat tcode
  -- 将t1组合成新的TModule
  catch ( do
    t2 <- forM rs $ \ex -> do
      case ex of
        Right t -> do
          case t of 
            TDef {} -> return []
            TConst {} -> return []
            TSchema {} -> return []
            TTable {} -> return []
            TEvent {} -> return []
            TImplements {} -> return []
            TModule {..} -> do
              let list = TList t3 TyAny _tInfo
              let mcode = (_mCode _tModuleDef)
              -- 将返回的tcode,再同一组合成一个code 
              let newCode = (_unCode mcode) <> codes
              let newModule = Module (_mName _tModuleDef) (_mKeySet _tModuleDef) (_mMeta _tModuleDef) (Code newCode) (_mHash _tModuleDef) 
                              (_mBlessed _tModuleDef) (_mInterfaces _tModuleDef)
              let tm = TModule newModule (abstract (const Nothing) list) _tInfo
              return [tm]
            -- vr@(TApp (TVar (QName qname _ _) _) _ _) -> do
            --   -- 如果是一段command，则加载其合约  (SealuaXXX/buy-token ..)
            --   p <- use (rEnv . eePactDbVar)
            --   -- 从数据库取出合约
            --   dbModule <- liftIO $ _readRow repldb Modules qname p    
            --   case dbModule of
            --     Nothing -> do
            --       --如果是系统合约，则加载?
            --       let (ModuleName moduleN) = qname
            --       if moduleN == "Token" then do
            --         rName .= Nothing
            --         _ <- parsedCompileEval tokenSrc tokenResult
            --         rEvalState.evalRefs.rsLoaded .= HM.empty
            --         refs1 <- use (rEvalState . evalRefs)
            --         rEnv.eeRefStore %= updateRefStore refs1
            --         return ()
            --       else do 
            --         --throw error
            --         let info = "token error:can't find module '" <> moduleN <> "' from db"
            --         throwM $ PactError EvalError def def info
            --     Just m  -> do
            --       case m of 
            --         Module {..} -> do
            --           --加载合约
            --           rName .= Nothing
            --           let strCode = Text.unpack (_unCode _mCode)
            --           let tokenRs = TF.parseString exprsOnly mempty strCode
            --           _ <- parsedCompileEval strCode tokenRs
            --           rEvalState.evalRefs.rsLoaded .= HM.empty
            --           refs1 <- use (rEvalState . evalRefs)
            --           rEnv.eeRefStore %= updateRefStore refs1
            --           return ()
            --         _         -> return ()
            --   return [vr]
            _         -> return [t]
        Left _ -> return []
    let t4 = concat t2
    result <- forM t4 $ \e -> pureEval (_tInfo e) (eval e)
    return $ last result)
    $ \(e :: PactError) -> do
      return (Left (show e))

getExpParsed :: TF.Result [Exp Parsed] -> [Exp Parsed]
getExpParsed (TF.Success es) = es
getExpParsed (TF.Failure _) = []

handleCompile :: String -> Exp Parsed -> Repl (Either String (Term Name))
handleCompile src exp = do
  -- outStrLn HOut (show exp)
  ns <- use rName
  case compile ns (mkStringInfo src) exp of
    Right t -> do
      case t of 
        (TModule (Module modName _ _ _ hh _ _) _ _) -> do
          rName .= Just (modName,hh)
          return (Right t)
        _ -> return (Right t)
    Left er -> do
        case _iInfo (peInfo er) of
          Just (_,d) -> do
                      -- mode <- use rMode
                      outStr HErr (renderPrettyString (RPlain) (_pDelta d))
                      outStrLn HErr $ ": error: " ++ unpack (peText er)
          Nothing -> outStrLn HErr $ "[No location]: " ++ unpack (peText er)
        return (Left $ show er)


pureEval :: Info -> Eval LibState (Term Name) -> Repl (Either String (Term Name))
pureEval ei e = do
  (ReplState evalE evalS _ _ _) <- get
  er <- try (liftIO $ runEval' evalS evalE e)
  let (r,es) = case er of
                 Left (SomeException ex) -> (Left (PactError EvalError def def (pack $ show ex)),evalS)
                 Right v -> v
  -- mode <- use rMode
  case r of
    Right a -> do
        doOut ei a
        rEvalState .= es
        updateForOp a
    Left err -> do
        serr <- renderErr err
        let cs = peCallStack err
        if null cs
          then outStrLn HErr serr
          else do
          -- synthesize error at outer callsite
          let lastErr = last cs
              outerErr = err { peInfo = _sfLoc lastErr }
          if peInfo outerErr == peInfo err
            then outStrLn HErr serr
            else do
            renderErr outerErr >>= outStrLn HErr
            outStrLn HErr (" at " ++ serr)
          mapM_ (\c -> outStrLn HErr $ " at " ++ show c) cs
        return (Left serr)

doOut :: Show t => Info -> t -> Repl ()
doOut _ a = plainOut
  where
    plainOut = outStrLn HOut (show a)

renderErr :: PactError -> Repl String
renderErr a
  | peInfo a == def = do
      let i = Info (Just (mempty,Parsed (Lines 0 0 0 0) 0))
      return $ renderInfo i ++ ":" ++ unpack (peText a)
  | otherwise = return $ renderInfo (peInfo a) ++ ": " ++ unpack (peText a)

updateForOp :: Term Name -> Repl (Either String (Term Name))
updateForOp a = do
  mv <- use (rEnv.eePactDbVar)
  -- mode <- use rMode
  op <- liftIO $ modifyMVar mv $ \v -> return (set rlsOp Noop v,view rlsOp v)
  case op of
    Noop -> return (Right a)
    UpdateEnv e -> do
      rEnv %= appEndo e
      return (Right a)
    Load _ _ -> do
                  -- when reset (initReplState mode >>= put >> void useReplLib)
                  (a <$) <$> parsedCompileEval tokenSrc tokenResult
    -- Load _ _ -> return (Right a)
    TcErrors es -> forM_ es (outStrLn HErr) >> return (Right a)
    Print t -> do
      let rep = case t of TLitString s -> unpack s
                          _ -> show t
      outStrLn HOut rep
      return (Right a)
    Tx i t n -> doTx i t n

doTx :: Info -> Tx -> Maybe Text -> Repl (Either String (Term Name))
doTx i t n = do
  e <- case t of
    Begin -> do
      rEnv.eeTxId %= fmap succ
      return $ evalBeginTx i
    Rollback -> return $ evalRollbackTx i
    Commit -> return $ void $ evalCommitTx i
  pureEval i (e >> return (tStr "")) >>= \r -> forM r $ \_ -> do
    case t of
      Commit -> do
        newmods <- use (rEvalState . evalRefs . rsNewModules)
        rEnv . eeRefStore . rsModules %= HM.union newmods
      _ -> return ()
    rEvalState .= def
    useReplLib
    tid <- use $ rEnv . eeTxId
    return $ tStr $ tShow t <> " Tx " <> tShow tid <> maybe "" (": " <>) n

isPactFile :: String -> Bool
isPactFile fp = endsWith fp ".seal"

-- | load and evaluate a Seal.Contract file.
-- Track file and use current file to mangle directory as necessary.
loadFile :: FilePath -> Repl (Either String (Term Name))
loadFile f = do
  curFileM <- use rFile
  let computedPath = case curFileM of
        Nothing -> f -- no current file, just use f
        Just curFile
          | isAbsolute f -> f -- absolute always wins
          | takeFileName curFile == curFile -> f -- current with no directory loses
          | otherwise -> combine (takeDirectory curFile) f -- otherwise start with dir of curfile
      restoreFile = rFile .= curFileM
  rFile .= Just computedPath
  catch (do
          pr <- TF.parseFromFileEx exprsOnly computedPath
          src <- liftIO $ readFile computedPath
          when (isPactFile f) $ rEvalState.evalRefs.rsLoaded .= HM.empty
          r <- parsedCompileEval src pr
          when (isPactFile f) $ void useReplLib
          restoreFile
          return r)
         $ \(e :: SomeException) -> do
               restoreFile
               outStrLn HErr $ "load: file load failed: " ++ f ++ ", " ++ show e
               return (Left (show e))

out :: Hdl -> Bool -> String -> Repl ()
out hdl newline str =
  liftIO $ do
    let h = case hdl of HOut -> stdout; HErr -> stderr
    (if newline then hPutStrLn else hPutStr) h str
    hFlush h


outStr :: Hdl -> String -> Repl ()
outStr h s = out h False s
outStrLn :: Hdl -> String -> Repl ()
outStrLn h s = out h True s


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


rSuccess :: Monad m => m (Either a (Term Name))
rSuccess = return $ Right $ toTerm True

-- | install repl lib functions into monad state
useReplLib :: Repl ()
useReplLib = id %= setReplLib

-- | mutate repl state to install lib functions
setReplLib :: ReplState -> ReplState
setReplLib = over (rEvalState.evalRefs.rsLoaded) $ HM.union (moduleToMap replDefs)

-- | mutate repl state to remove lib functions
unsetReplLib :: ReplState -> ReplState
unsetReplLib = over (rEvalState.evalRefs.rsLoaded) (`HM.difference` (moduleToMap replDefs))

-- | evaluate string in repl monad
evalPact :: String -> Repl (Either String (Term Name))
evalPact cmd = parsedCompileEval cmd (TF.parseString exprsOnly mempty cmd)

-- | evaluate string in repl monad, loading lib functions first.
evalRepl' :: String -> Repl (Either String (Term Name))
-- evalRepl' cmd = useReplLib >> evalPact cmd
evalRepl' cmd = evalPact cmd

-- evalRepl :: ReplMode -> String -> IO (Either String (Term Name))
-- evalRepl m cmd = initReplState m >>= evalStateT (evalRepl' cmd)

evalString :: ReplState -> [Account] -> String -> IO (Value, ReplState)
evalString rs accounts cmd  = do
  (er,s) <- flip runStateT rs $ do
    -- 加载系统合约
    _ <- parsedCompileEval tokenSrc tokenResult
    rName .= Nothing
    -- _ <- parsedCompileEval tokenSrc2 tokenResult2
    -- 如何判断是一个合约，还是一段command,还是不用区分？需要区分，一个可以执行repl libs，一个不能执行repl libs。合约会不会动到其他合约的表
    -- 开放了合约就所以功能都可以用了 不需要区分？权限再做考虑？
    -- 先加载系统合约，再加载用户合约/命令 ()
    -- 不支持用户合约直接的互相调用，只支持用户合约调用系统合约
    -- 如何拆分命令，加载module (SEALuat7yPXvZabms25pLq69jFJyHzP8vUxwsxWQi7PysTH2Eh5mPWSwiLYfEBb.helloWorld/hello "")
    -- 1.解析出moduleName:SEALuat7yPXvZabms25pLq69jFJyHzP8vUxwsxWQi7PysTH2Eh5mPWSwiLYfEBb.helloWorld 
    -- 2.通过moduleName,从DB加载合约
    -- 3.再执行command命令

    -- 加载合约过程
    -- 1.执行parseCompileEval

    -- 能够通过/拆分就是command?

    -- lines cmd，如果有多条记录就是contract?合约里面包含command怎么处理？
    let sigs = S.fromList $ map account2PublicKey accounts
    rEvalState.evalRefs.rsLoaded .= HM.empty
    refs <- use (rEvalState . evalRefs)
    rEnv.eeRefStore %= updateRefStore refs
  
            
    --将用户签名替换adminkey
    rEnv.eeMsgSigs .= sigs
    -- (ReplState fEnv _ fOut fPath) <- get
    -- liftIO $ print evalstate
    -- liftIO $ print fOut
    -- liftIO $ print fPath
    -- let eeStore = _eeRefStore fEnv
    -- liftIO $ print refs
    liftIO $ print ("Execute Command:" ++ cmd)
    evalRepl' cmd
  result <- case er of 
    Right v -> return ["success" A..= v]
    -- Left e  -> return ["failed" A..= e]
    Left e  -> throwM $ PactError EvalError def def $ Text.pack $ "token error:" ++  e
  return $ (object $ result, s)

-- _genAppleEnv :: MPDB -> MPDB -> IO ApplyEnv
-- _genAppleEnv accMpdb pactMpdb = do
--   return ApplyEnv {
--     _aeAccountMPDB = accMpdb
--    ,_aePactMPDB = pactMpdb 
-- }

-- _genApplyState :: MPDB -> IO ApplyState
-- _genApplyState pactMpdb = do
--   return ApplyState {
--     _asAcctModifier = mempty
--    ,_asPactModifier = MPtreeDb def pactMpdb
-- }


-- _genPactMpdb :: IO MPDB
-- _genPactMpdb = do
--   db' <- openRocksDB "/tmp/seal1"
--   let rdb = MPDB {rdb=db',stateRoot=emptyTriePtr}
--   initializeBlank rdb
--   return rdb

-- _genAccMpdb :: IO MPDB
-- _genAccMpdb = do
--   db' <- openRocksDB "/tmp/seal2"
--   let rdb = MPDB {rdb=db',stateRoot=emptyTriePtr}
--   initializeBlank rdb
--   return rdb

-- _sealTx :: TxId
-- _sealTx = undefined



-- _runApply2 :: ApplyState -> ApplyEnv -> [String] -> [Account] -> IO (Either String (),ApplyState)
-- _runApply2 applySt appEnv cmds accounts = do
--   runApply applySt appEnv $ do
--     applyCommands _sealTx cmds accounts