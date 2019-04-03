{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  Seal.Contract.Compile
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Compiler from 'Exp' -> 'Term Name'
--

module Seal.Contract.Compile
    (
     compile,compileExps
    ,MkInfo,mkEmptyInfo,mkStringInfo,mkTextInfo
    )

where

import qualified Text.Trifecta as TF hiding (expected)
import Control.Applicative hiding (some,many)
import Text.Megaparsec as MP
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Arrow ((&&&),first)
import Prelude hiding (exp)
import Bound
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Control.Exception hiding (try)
import Data.String
import Control.Lens hiding (prism)
import Data.Maybe
import Data.Default
import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS

import Seal.Contract.Types.ExpParser
import Seal.Contract.Types.Exp
import Seal.Contract.Parse (exprsOnly,parseExprs)
import Seal.Contract.Types.Hash
import Seal.Contract.Types.Term
import Seal.Contract.Types.Util
import Seal.Contract.Types.Info
import Seal.Contract.Types.Type
import Seal.Contract.Types.Runtime (PactError)

import Seal.Core.Common.Account (isTextAccount)

data CompileState = CompileState
  { _csFresh :: Int
  , _csModule :: Maybe (ModuleName,Hash)
  }
makeLenses ''CompileState

type Compile a = ExpParse CompileState a

initParseState :: Maybe (ModuleName,Hash) -> Exp Info -> ParseState CompileState
initParseState name e = ParseState e $ CompileState 0 name


reserved :: [Text]
reserved =
  T.words "use defcontract defn step step-with-rollback true false let let* defconst interface implements ns"

compile :: Maybe (ModuleName,Hash) -> MkInfo -> Exp Parsed -> Either PactError (Term Name)
compile name mi e = let ei = mi <$> e in runCompile term (initParseState name ei) ei

compileExps :: Traversable t => MkInfo -> t (Exp Parsed) -> Either PactError (t (Term Name))
compileExps mi exps = sequence $ compile Nothing mi <$> exps

currentModule :: Compile (ModuleName,Hash)
currentModule = use (psUser . csModule) >>= \m -> case m of
  Just cm -> return cm  
  Nothing -> context >>= tokenErr' "Must be declared after ns"

currentModule' :: Compile ModuleName
currentModule' = fst <$> currentModule

freshTyVar :: Compile (Type (Term Name))
freshTyVar = do
  c <- state (view (psUser . csFresh) &&& over (psUser . csFresh) succ)
  return $ mkTyVar (cToTV c) []

cToTV :: Int -> TypeVarName
cToTV n | n < 26 = fromString [toC n]
        | n <= 26 * 26 = fromString [toC (pred (n `div` 26)), toC (n `mod` 26)]
        | otherwise = fromString $ toC (n `mod` 26) : show ((n - (26 * 26)) `div` 26)
  where toC i = toEnum (fromEnum 'a' + i)


term :: Compile (Term Name)
term =
  literal
  <|> varAtom
  <|> withList' Parens
    ((specialForm <|> app <|> keyget) <* eof)
  <|> listLiteral
  <|> objectLiteral

keyget :: Compile (Term Name)
keyget = do
  body <- many term
  case body of
    [k@(TLiteral (LKeyword _) _), o@TObject {}] ->
      TApp (TVar (Name "get" def) def) [o, k] <$> contextInfo
    [o@TObject {}, k@(TLiteral (LKeyword _) _)] ->
      TApp (TVar (Name "get" def) def) [o, k] <$> contextInfo
    [k@(TLiteral (LKeyword _) _), o@TObject {}, defValue] ->
      TApp (TVar (Name "get" def) def) [o, k, defValue] <$> contextInfo
    [o@TObject {}, k@(TLiteral (LKeyword _) _), defValue] ->
      TApp (TVar (Name "get" def) def) [o, k, defValue] <$> contextInfo
    _ -> expected "special key get"



-- | User-available atoms (excluding reserved words).
userAtom :: Compile (AtomExp Info)
userAtom = do
  a@AtomExp{..} <- bareAtom
  when (_atomAtom `elem` reserved) $ unexpected' "reserved word"
  pure a

specialForm :: Compile (Term Name)
specialForm = do
  vatom <- bareAtom 
  return vatom >>= \AtomExp{..} -> case _atomAtom of
    "use" -> commit >> useForm
    "ns" -> commit >> nsForm
    "let" -> commit >> letsForm
    "def" -> commit >> defconst
    "bless" -> commit >> bless
    "deftable" -> commit >> deftable
    "defrecord" -> commit >> defschema
    "defevent" -> commit >> defevent
    "defn" -> commit >> defun PUBLIC
    "defn-" -> commit >> defun PRIVATE
    "interface" -> commit >> interface
    "implements" -> commit >> implements
    "with-read" -> commit >> withRead (expToTerm vatom)
    "with-default-read" -> commit >> withDefaultRead (expToTerm vatom)
    _ -> expected "special form"

expToTerm :: AtomExp Info -> Term Name
expToTerm AtomExp{..} = TVar (Name _atomAtom _atomInfo) _atomInfo

withRead :: Term Name -> Compile (Term Name)
withRead v = do
  tbl <- term
  key <- term
  bdn <- bindingForm
  TApp v [tbl, key, bdn] <$> contextInfo

withDefaultRead :: Term Name -> Compile (Term Name)
withDefaultRead v = do
  tbl <- term
  key <- term
  dfv <- objectLiteral
  bdn <- bindingForm
  TApp v [tbl, key, dfv, bdn] <$> contextInfo


app :: Compile (Term Name)
app = do
  v <- varAtom
  body <- many (term <|> bindingForm)
  TApp v body <$> contextInfo

-- | Bindings (`{ "column" := binding }`) do not syntactically scope the
-- following body form as a sexp, instead letting the body contents
-- simply follow, showing up as more args to the containing app. Thus, once a
-- binding is encountered, all following terms are subsumed into the
-- binding body, and bound/abstracted etc.
bindingForm :: Compile (Term Name)
bindingForm = do
  let pair = do
        a <-  arg
        col <- term
        -- a <- sep ColonEquals *> arg
        return (a,col)
  (bindings,bi) <- withList' Braces $
    (,) <$> pair `sepBy` sep Comma <*> contextInfo
  TBinding bindings <$> abstractBody (map fst bindings) <*>
    pure (BindSchema TyAny) <*> pure bi

varAtom :: Compile (Term Name)
varAtom = do
  AtomExp{..} <- atom
  -- _ <- expected  (show _atomAtom)

  if (_atomAtom `elem` division)
    then do
      let n = Name _atomAtom _atomInfo
      commit 
      return $ TVar n _atomInfo
    else do
      let divi = pack "/"
      let as = T.splitOn divi _atomAtom
      case as of 
        [sa] -> do 
          -- let sa = pack $ reverse (unpack ssa) 
          when (sa `elem` reserved) $ unexpected' "reserved word"
          if (isAccount sa) then
            if isTextAccount sa
              then do
                let n = LString sa
                commit
                return $ TLiteral n _atomInfo
              else  unexpected' "Account decode failed"
          else do 
            let n = Name sa _atomInfo
            commit
            return $ TVar n _atomInfo
        [q,a] -> do 
          when (q `elem` reserved) $ unexpected' "reserved word"
          when (a `elem` reserved) $ unexpected' "reserved word"
          let n = QName (ModuleName q) a _atomInfo
          commit
          return $ TVar n _atomInfo
        _ -> expected "single qualifier"

division :: [Text]
division =
  T.words "/"

isAccount :: Text -> Bool
isAccount t = if prefix == "SEALua" then True else False
  where prefix = T.take 6 t


listLiteral :: Compile (Term Name)
listLiteral = withList Brackets $ \ListExp{..} -> do
  ls <- case _listList of
    _ : CommaExp : _ -> term `sepBy` sep Comma
    _                -> many term
  let lty = case nub (map typeof ls) of
              [Right ty] -> ty
              _ -> TyAny
  pure $ TList ls lty _listInfo
  
objectLiteral :: Compile (Term Name)
objectLiteral =
  withList Braces $ \ListExp {..} -> do
    let pair = do
          key <- term
          val <- term
          return (key, val)
    ps <-
      case _listList of
        _:_:CommaExp:_:_ -> pair `sepBy` sep Comma
        _ -> many pair
    return $ TObject ps TyAny _listInfo

literal :: Compile (Term Name)
literal = lit >>= \LiteralExp{..} ->
  commit >> return (TLiteral _litLiteral _litInfo)


deftable :: Compile (Term Name)
deftable = do
  (mn,mh) <- currentModule
  AtomExp{..} <- userAtom
  ty <- optional (typed >>= \t -> case t of
                     TyUser {} -> return t
                     _ -> expected "user type")
  m <- meta ModelNotAllowed
  TTable (TableName _atomAtom) mn mh
    (fromMaybe TyAny ty) m <$> contextInfo


bless :: Compile (Term Name)
bless = TBless <$> hash' <*> contextInfo

defconst :: Compile (Term Name)
defconst = do
  modName <- currentModule'
  a <- arg
  v <- term

  m <- meta ModelNotAllowed
  TConst a modName (CVRaw v) m <$> contextInfo

data ModelAllowed
  = ModelAllowed
  | ModelNotAllowed

meta :: ModelAllowed -> Compile Meta
meta modelAllowed = atPairs <|> try docStr <|> return def
  where
    docStr = Meta <$> (Just <$> str) <*> pure []
    docPair = symbol "@doc" >> str
    modelPair = do
      symbol "@model"
      (ListExp exps _ _i, _) <- list' Brackets
      pure exps
    atPairs = do
      doc <- optional (try docPair)
      model <- optional (try modelPair)
      case (doc, model, modelAllowed) of
        (Nothing, Nothing    , ModelAllowed   ) -> expected "@doc or @model declarations"
        (Nothing, Nothing    , ModelNotAllowed) -> expected "@doc declaration"
        (_      , Just model', ModelAllowed   ) -> return (Meta doc model')
        (_      , Just _     , ModelNotAllowed) -> syntaxError "@model not allowed in this declaration"
        (_      , Nothing    , _              ) -> return (Meta doc [])

defschema :: Compile (Term Name)
defschema = do
  modName <- currentModule'
  tn <- _atomAtom <$> userAtom
  m <- meta ModelAllowed
  fields <- withList' Brackets $ many arg
  TSchema (TypeName tn) modName m fields <$> contextInfo

defevent :: Compile (Term Name)
defevent = do
  modName <- currentModule'
  tn <- _atomAtom <$> userAtom
  m <- meta ModelAllowed
  fields <- withList' Brackets $ many arg
  TEvent (TypeName tn) modName m fields <$> contextInfo

defun :: DefVisibility -> Compile (Term Name)
defun visibility = do
  modName <- currentModule'
  (defname,returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Brackets $ many arg --[]
  m <- meta ModelAllowed
  TDef visibility defname modName Defun (FunType args returnTy)
    <$> abstractBody args <*> pure m <*> contextInfo

implements :: Compile (Term Name)
implements = do
  modName <- currentModule'
  ifName <- (ModuleName . _atomAtom) <$> bareAtom
  info <- contextInfo
  return $ TImplements ifName modName info

interface :: Compile (Term Name)
interface = do
  iname' <- _atomAtom <$> bareAtom
  m <- meta ModelAllowed
  use (psUser . csModule) >>= \ci -> case ci of
    Just {} -> syntaxError "invalid nested interface or contract"
    Nothing -> return ()
  info <- contextInfo
  let code = case info of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      iname = ModuleName iname'
      ihash = hash $ encodeUtf8 (_unCode code)
  (psUser . csModule) .= Just (iname, ihash)
  (defs, defInfo) <- interfaceForm
  return $ TModule
    (Interface iname code m)
    (abstract (const Nothing) (TList defs TyAny defInfo)) info

interfaceForm :: Compile ([Term Name], Info)
interfaceForm = (,) <$> some interfaceForms <*> contextInfo
  where
    interfaceForms = withList' Parens $ do
      AtomExp{..} <- bareAtom
      case _atomAtom of
        "defn" -> commit >> emptyDef
        "def" -> commit >> defconst
        "use" -> commit >> useForm
        t -> syntaxError $ "Invalid interface declaration: " ++ unpack t

emptyDef :: Compile (Term Name)
emptyDef = do
  modName <- currentModule'
  (defName, returnTy) <- first _atomAtom <$> typedAtom
  args <- withList' Parens $ many arg
  m <- meta ModelAllowed
  info <- contextInfo
  return $
    TDef PUBLIC defName modName Defun
    (FunType args returnTy) (abstract (const Nothing) (TList [] TyAny info)) m info


-- step :: Compile (Term Name)
-- step = do
--   cont <- try (TStep <$> (Just <$> term) <*> term) <|>
--           (TStep Nothing <$> term)
--   cont <$> pure Nothing <*> contextInfo

-- stepWithRollback :: Compile (Term Name)
-- stepWithRollback = do
--   try (TStep <$> (Just <$> term) <*> term <*> (Just <$> term) <*> contextInfo) <|>
--       (TStep Nothing <$> term <*> (Just <$> term) <*> contextInfo)



letBindings :: Compile [(Arg (Term Name),Term Name)]
letBindings = withList' Brackets $ -- []
              some $
              (,) <$> arg <*> term

abstractBody :: [Arg (Term Name)] -> Compile (Scope Int Term Name)
abstractBody args = abstractBody' args <$> bodyForm

abstractBody' :: [Arg (Term Name)] -> Term Name -> Scope Int Term Name
abstractBody' args = abstract (`elemIndex` bNames)
  where bNames = map arg2Name args


-- letForm :: Compile (Term Name)
-- letForm = do
--   bindings <- letBindings
--   TBinding bindings <$> abstractBody (map fst bindings) <*>
--     pure BindLet <*> contextInfo

-- | let* is a macro to nest lets for referencing previous
-- bindings.
letsForm :: Compile (Term Name)
letsForm = do
  bindings <- letBindings
  let nest (binding:rest) = do
        let bName = [arg2Name (fst binding)]
        scope <- abstract (`elemIndex` bName) <$> case rest of
          [] -> bodyForm
          _ -> do
            rest' <- nest rest
            pure $ TList [rest'] TyAny def
        TBinding [binding] scope BindLet <$> contextInfo
      nest [] =  syntaxError "letsForm: invalid state (bug)"
  nest bindings

useForm :: Compile (Term Name)
useForm = do
  modName <- (_atomAtom <$> userAtom) <|> str <|> expected "bare atom, string, symbol"
  TUse (ModuleName modName) <$> optional hash' <*> contextInfo

nsForm :: Compile (Term Name)
nsForm = do
  modName' <- _atomAtom <$> userAtom
  --如果是用户合约，则直接用ns名称作key
  keyset <- if modName' == "Token" then do
              keyset' <- str
              return keyset'
            else do 
              syntaxError "ns: not support custom contract"
              -- let ts = T.split (=='.') modName'
              -- return (head ts)
  m <- meta ModelAllowed
  use (psUser . csModule) >>= \cm -> case cm of
    Just {} -> syntaxError "Invalid nested contract or interface"
    Nothing -> return ()
  i <- contextInfo
  let code = case i of
        Info Nothing -> "<code unavailable>"
        Info (Just (c,_)) -> c
      modName = ModuleName modName'
      modHash = hash $ encodeUtf8 $ _unCode code
      blessed = HS.fromList []
  (psUser . csModule) .= Just (modName,modHash)
  return $ TModule
    (Module modName (KeySetName keyset) m code modHash blessed [])
    (abstract (const Nothing) (TList [] TyAny i)) i

hash' :: Compile Hash
hash' = str >>= \s -> case fromText' s of
  Right h -> return h
  Left e -> syntaxError $ "bad hash: " ++ e

typedAtom :: Compile (AtomExp Info,Type (Term Name))
typedAtom = flip (,) <$> (typed <|> freshTyVar) <*> userAtom

arg :: Compile (Arg (Term Name))
arg = typedAtom >>= \(AtomExp{..},ty) ->
  return $ Arg _atomAtom ty _atomInfo

arg2Name :: Arg n -> Name
arg2Name Arg{..} = Name _aName _aInfo


typed :: Compile (Type (Term Name))
typed = sep SCaret *> parseType

parseType :: Compile (Type (Term Name))
parseType = msum
  [ parseListType
  , parseUserSchemaType
  , parseSchemaType tyObject TyObject
  , parseSchemaType tyTable TyTable
  , TyPrim TyInteger <$ symbol tyInteger
  , TyPrim TyDecimal <$ symbol tyDecimal
  , TyPrim TyTime    <$ symbol tyTime
  , TyPrim TyBool    <$ symbol tyBool
  , TyPrim TyString  <$ symbol tyString
  , TyList TyAny     <$ symbol tyList
  , TyPrim TyValue   <$ symbol tyValue
  , TyPrim TyKeySet  <$ symbol tyKeySet
  , TyPrim TyString  <$ symbol tyAddress
  , TyPrim TyKeyword  <$ symbol tyKeyword
  ]

parseListType :: Compile (Type (Term Name))
parseListType = withList' Brackets $ TyList <$> parseType

parseSchemaType :: Text -> SchemaType -> Compile (Type (Term Name))
parseSchemaType tyRep sty = symbol tyRep >>
  (TySchema sty <$> (parseUserSchemaType <|> pure TyAny))


parseUserSchemaType :: Compile (Type (Term Name))
parseUserSchemaType = withList Braces $ \ListExp{..} -> do
  AtomExp{..} <- userAtom
  return $ TyUser (return $ Name _atomAtom _listInfo)

bodyForm :: Compile (Term Name)
bodyForm = do
  (bs,i) <- bodyForm'
  return $ TList bs TyAny i

bodyForm' :: Compile ([Term Name],Info)
bodyForm' = (,) <$> some term <*> contextInfo

_compileAccounts :: IO (Either PactError [Term Name])
_compileAccounts = _parseF "examples/accounts/accounts.seal" >>= _compile id

_compile :: (ParseState CompileState -> ParseState CompileState) ->
            TF.Result ([Exp Parsed],String) -> IO (Either PactError [Term Name])
_compile _ (TF.Failure f) = putDoc (TF._errDoc f) >> error "Parse failed"
_compile sfun (TF.Success (a,s)) = return $ forM a $ \e ->
  let ei = mkStringInfo s <$> e
  in runCompile term (sfun (initParseState Nothing ei)) ei

-- | run a string as though you were in a contract (test deftable, etc)
_compileStrInModule :: String -> IO [Term Name]
_compileStrInModule = _compileStr' (set (psUser . csModule) (Just ("mycontract",hash mempty)))

_compileStr :: String -> IO [Term Name]
_compileStr = _compileStr' id

_compileStr' :: (ParseState CompileState -> ParseState CompileState) -> String -> IO [Term Name]
_compileStr' sfun code = do
    r <- _compile sfun ((,code) <$> _parseS code)
    case r of Left e -> throwIO $ userError (show e)
              Right t -> return t

_parseS :: String -> TF.Result [Exp Parsed]
_parseS = TF.parseString exprsOnly mempty

_parseF :: FilePath -> IO (TF.Result ([Exp Parsed],String))
_parseF fp = readFile fp >>= \s -> fmap (,s) <$> TF.parseFromFileEx exprsOnly fp

_compileFile :: FilePath -> IO [Term Name]
_compileFile f = do
    p <- _parseF f
    rs <- case p of
            (TF.Failure e) -> putDoc (TF._errDoc e) >> error "Parse failed"
            (TF.Success (es,s)) -> return $ map (compile Nothing (mkStringInfo s)) es
    case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts

_atto :: FilePath -> IO [Term Name]
_atto fp = do
  f <- pack <$> readFile fp
  rs <- case parseExprs f of
    Left s -> throwIO $ userError s
    Right es -> return $ map (compile Nothing (mkStringInfo (unpack f))) es
  case sequence rs of
      Left e -> throwIO $ userError (show e)
      Right ts -> return ts

_testCToTV :: Bool
_testCToTV = nub vs == vs where vs = take (26*26*26) $ map cToTV [0..]
