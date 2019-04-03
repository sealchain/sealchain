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

module Seal.Contract.Native
    (natives
    ,nativeDefs
    ,langDefs
    ,moduleToMap
    ,lengthDef
    ,enforceDef
    ,enforceOneDef
    ,pactVersionDef
    ,formatDef
    ,hashDef
    ,ifDef
    ,readDecimalDef
    ,lookupObj
    ) where

import Control.Lens hiding (parts,Fold,contains)
import Control.Monad
import Control.Monad.Reader (asks)
import Control.Monad.Catch

import Data.Char (isHexDigit, digitToInt)
import Data.Default
import qualified Data.Attoparsec.Text as AP
import Prelude
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T (isInfixOf, length, all, splitOn, null, foldl', append)
import Safe
import Control.Arrow hiding (app)
import Data.Foldable
import Data.Aeson hiding ((.=))
import Data.Decimal
import Data.List
import Data.Function (on)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BSC

import Seal.Contract.Eval
import Seal.Contract.Native.Db
import Seal.Contract.Native.Internal
import Seal.Contract.Native.Time
import Seal.Contract.Native.Ops
import Seal.Contract.Native.Keysets
import Seal.Contract.Types.Runtime
import Seal.Contract.Parse
import Seal.Contract.Types.Version
import Seal.Contract.Types.Hash
import qualified Data.Set as S
import Universum ((<>))

-- | All production native modules.
natives :: [NativeModule]
natives = [
  langDefs,
  dbDefs,
  timeDefs,
  opDefs,
  keyDefs]


-- | Production native modules as a dispatch map.
nativeDefs :: M.HashMap Name Ref
nativeDefs = mconcat $ map moduleToMap natives

moduleToMap :: NativeModule -> M.HashMap Name Ref
moduleToMap = M.fromList . map (((`Name` def) . asString) *** Direct) . snd


lengthDef :: NativeDef
lengthDef = defRNative "count" count' (funType tTyInteger [("x",listA)])
 "Compute length of X, which can be a list, a string, or an object.\
 \`(count [1 2 3])` `(count \"abcdefgh\")` `(count { \"a\": 1, \"b\": 2 })`"

listA :: Type n
listA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString,TySchema TyObject (mkSchemaVar "o")]

enforceDef :: NativeDef
enforceDef = defNative "enforce" enforce
  (funType tTyBool [("test",tTyBool),("msg",tTyString)])
  "Fail transaction with MSG if pure expression TEST is false. Otherwise, returns true. \
  \`!(enforce (!= (+ 2 2) 4) \"Chaos reigns\")`"
  where

    enforce :: NativeFun e
    enforce i as = runPure $ gasUnreduced i as $ mapM reduce as >>= enforce' i

    enforce' :: RNativeFun e
    enforce' i [TLiteral (LBool b) _,TLitString msg]
        | b = return $ TLiteral (LBool True) def
        | otherwise = failTx (_faInfo i) $ unpack msg
    enforce' i as = argsError i as
    {-# INLINE enforce' #-}

enforceOneDef :: NativeDef
enforceOneDef =
  defNative "enforce-one" enforceOne (funType tTyBool [("msg",tTyString),("tests",TyList tTyBool)])
  "Run TESTS in order (in pure context, plus keyset enforces). If all fail, fail transaction. Short-circuits on first success. \
  \`(enforce-one \"Should succeed on second test\" [(enforce false \"Skip me\") (enforce (= (+ 2 2) 4) \"Chaos reigns\")])`"
  where

    enforceOne :: NativeFun e
    enforceOne i as@[msg,TList conds _ _] = runPureSys (_faInfo i) $
      gasUnreduced i as $ do
        msg' <- reduce msg >>= \m -> case m of
          TLitString s -> return s
          _ -> argsError' i as
        let tryCond r@Just {} _ = return r
            tryCond Nothing cond = catch
              (Just <$> reduce cond)
              (\(_ :: SomeException) -> return Nothing)
        r <- foldM tryCond Nothing conds
        case r of
          Nothing -> failTx (_faInfo i) (unpack msg')
          Just b -> return b
    enforceOne i as = argsError' i as

pactVersionDef :: NativeDef
pactVersionDef = setTopLevelOnly $ defRNative "seal-version"
  (\_ _ -> return $ toTerm pactVersion)
  (funType tTyString [])
  "Obtain current seal build version. `(seal-version)`"


formatDef :: NativeDef
formatDef =
  defRNative "format" format
  (funType tTyString [("template",tTyString),("vars",TyList TyAny)])
  "Interpolate VARS into TEMPLATE using {}. \
  \`(format \"My {} has {}\" [\"dog\" \"fleas\"])`"
  where

    format :: RNativeFun e
    format i [TLitString s,TList es _ _] = do
      let parts = T.splitOn "{}" s
          plen = length parts
          rep (TLitString t) = t
          rep t = pack $ show t
      if plen == 1
      then return $ tStr s
      else if plen - length es > 1
           then evalError' i "format: not enough arguments for template"
           else return $ tStr $
                foldl'
                  (\r (e,t) -> r <> rep e <> t)
                  (head parts)
                  (zip es (tail parts))
    format i as = argsError i as

hashDef :: NativeDef
hashDef = defRNative "hash" hash' (funType tTyString [("value",a)])
  "Compute BLAKE2b 512-bit hash of VALUE. Strings are converted directly while other values are \
  \converted using their JSON representation. `(hash \"hello\")` `(hash { 'foo: 1 })`"
  where
    hash' :: RNativeFun e
    hash' i as = case as of
      [TLitString s] -> go $ encodeUtf8 s
      [a'] -> go $ toStrict $ encode a'
      _ -> argsError i as
      where go = return . tStr . asString . hash

ifDef :: NativeDef
ifDef = defNative "if" if' (funType a [("cond",tTyBool),("then",a),("else",a)])
  "Test COND. If true, evaluate THEN. Otherwise, evaluate ELSE. \
  \`(if (= (+ 2 2) 4) \"Sanity prevails\" \"Chaos reigns\")`"
  where

    if' :: NativeFun e
    if' i as@[cond,then',else'] = gasUnreduced i as $ reduce cond >>= \case
               TLiteral (LBool c) _ -> reduce (if c then then' else else')
               t -> evalError' i $ "if: conditional not boolean: " ++ show t
    if' i as = argsError' i as


readDecimalDef :: NativeDef
readDecimalDef = defRNative "read-decimal" readDecimal
  (funType tTyDecimal [("key",tTyString)])
  "Parse KEY string or number value from top level of message data body as decimal.\
  \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"
  where

    readDecimal :: RNativeFun e
    readDecimal i [TLitString key] = do
      (ParsedDecimal a') <- parseMsgKey i "read-decimal" key
      return $ toTerm a'
    readDecimal i as = argsError i as


langDefs :: NativeModule
langDefs =
    ("General",[
     ifDef
    ,defNative "map" map'
     (funType (TyList a) [("app",lam b a),("list",TyList b)])
     "Apply APP to each element in LIST, returning a new list of results. \
     \`(map (+ 1) [1 2 3])`"

     ,defRNative "empty?" empty'
     (funType (TyList a) [("app",lam b a),("list",TyList b)])
     "Apply APP to each element in LIST, returning a new list of results. \
     \`(map (+ 1) [1 2 3])`"

    ,defNative "reduce" reduce'
     (funType a [("app",lam2 a b a),("init",a),("list",TyList b)])
     "Iteratively reduce LIST by applying APP to last result and element, starting with INIT. \
     \`(reduce (+) 0 [100 10 5])`"

    ,defRNative "list" list
     (funType (TyList TyAny) [("elems",TyAny)])
     "Create list from ELEMS. Deprecated in Seal.Contract 2.1.1 with literal list support. `(list 1 2 3)`"

    ,defRNative "make-array" makeArray (funType (TyList a) [("value",a),("length",tTyInteger)])
     "Create list by repeating VALUE LENGTH times. `(make-array true 5)`"

    ,defRNative "reverse" reverse' (funType (TyList a) [("list",TyList a)])
     "Reverse LIST. `(reverse [1 2 3])`"

    ,defNative "filter" filter'
     (funType (TyList a) [("app",lam a tTyBool),("list",TyList a)])
     "Filter LIST by applying APP to each element. For each true result, the original value is kept.\
     \`(filter (comp (count) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

    ,defRNative "sort" sort'
     (funType (TyList a) [("values",TyList a)] <>
      funType (TyList (tTyObject (mkSchemaVar "o"))) [("fields",TyList tTyString),("values",TyList (tTyObject (mkSchemaVar "o")))])
     "Sort a homogeneous list of primitive VALUES, or objects using supplied FIELDS list. \
     \`(sort [3 1 2])` `(sort ['age] [{'name: \"Lin\",'age: 30} {'name: \"Val\",'age: 25}])`"

    ,defNative (specialForm Where) where'
     (funType tTyBool [("field",tTyString),("app",lam a tTyBool),("value",tTyObject (mkSchemaVar "row"))])
     "Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE. \
     \`(filter (where 'age (> 20)) [{'name: \"Mary\",'age: 30} {'name: \"Juan\",'age: 15}])`"

     ,defNative "comp" comp' (funType a [("value",a)] <>
                             funType b [("appA", lam a b),("value",a)] <>
                             funType c [("appA", lam b c),("appB",lam a b),("value",a)] <>
                             funType d [("appA", lam c d),("appB", lam b c),("appC",lam a b),("value",a)]
                             )
     "Compose X and Y, such that X operates on VALUE, and Y on the results of X. \
     \`(filter (comp (count) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])`"

     ,lengthDef

    ,defRNative "take" take' takeDrop
     "Take COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, take from end. \
     \`(take 2 \"abcd\")` `(take (- 3) [1 2 3 4 5])` `(take ['name] { 'name: \"Vlad\", 'active: false})`"

    ,defRNative "drop" drop' takeDrop
     "Drop COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, drop from end.\
     \`(drop 2 \"vwxyz\")` `(drop (- 2) [1 2 3 4 5])` `(drop ['name] { 'name: \"Vlad\", 'active: false})`"

     ,defRNative "assoc" assoc' (funType (tTyObject (mkSchemaVar "o")) [("object",tTyObject (mkSchemaVar "o")),("key",tTyString),("value",a)])
     "Remove entry for KEY from OBJECT. `(assoc { :foo 1 } :bar 2)`"

    ,defRNative "dissoc" dissoc' (funType (tTyObject (mkSchemaVar "o")) [("object",tTyObject (mkSchemaVar "o")),("key",tTyString)])
     "Remove entry for KEY from OBJECT. `(dissoc { :foo 1, :bar 2 } :bar)`"

    ,defRNative "get" get' (funType a [("list",TyList (mkTyVar "l" [])),("idx",tTyInteger)] <>
                            funType a [("list",TyList (mkTyVar "l" [])),("idx",tTyInteger),("defValue",a)] <>
                            funType a [("object",tTyObject (mkSchemaVar "o")),("idx",tTyString)]<>
                            funType a [("object",tTyObject (mkSchemaVar "o")),("idx",tTyString),("defValue",a)])
     "Index LIST at IDX, or get value with key IDX from OBJECT. \
     \`(get [1 2 3] 1)` `(get { :foo 1, :bar 2 } :bar)`"

    ,enforceDef
    ,enforceOneDef

    ,formatDef

    ,defRNative "seal-id" pactId (funType tTyInteger [])
     "Return ID if called during current seal execution, failing if not."

    ,readDecimalDef
    ,defRNative "read-integer" readInteger (funType tTyInteger [("key",tTyString)])
     "Parse KEY string or number value from top level of message data body as integer. `$(read-integer \"age\")`"
    ,defRNative "read-msg" readMsg (funType a [] <> funType a [("key",tTyString)])
     "Read KEY from top level of message data body, or data body itself if not provided. \
     \Coerces value to their corresponding seal type: String -> string, Number -> integer, Boolean -> bool, \
     \List -> list, Object -> object. However, top-level values are provided as a 'value' JSON type. \
     \`$(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))`"

    ,defRNative "tx-hash" txHash (funType tTyString [])
     "Obtain hash of current transaction as a string. `(tx-hash)`"
    
    ,defRNative "MSG_SENDER" msgSender (funType tTyString [])
     "Return the first PublicKey" 

     ,defRNative "MSG_VALUE" msgValue (funType tTyString [])
     "Return the msg.value"
    
    ,defNative (specialForm Bind) bind
     (funType a [("src",tTyObject row),("binding",TySchema TyBinding row)])
     "Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements. \
     \`(bind { \"a\": 1, \"b\": 2 } { \"a\" := a-value } a-value)`"
    ,defRNative "type" typeof'' (funType tTyString [("x",a)])
     "Returns type of X as string. `(type \"hello\")`"
    ,setTopLevelOnly $ defRNative "list-modules" listModules
     (funType (TyList tTyString) []) "List modules available for loading."
    ,defRNative "yield" yield (funType yieldv [("OBJECT",yieldv)])
     "Yield OBJECT for use with 'resume' in following seal step. The object is similar to database row objects, in that \
     \only the top level can be bound to in 'resume'; nested objects are converted to opaque JSON values. \
     \`$(yield { \"amount\": 100.0 })`"
    ,defNative "resume" resume
     (funType a [("binding",TySchema TyBinding (mkSchemaVar "y")),("body",TyAny)])
     "Special form binds to a yielded object value from the prior step execution in a seal."

    ,pactVersionDef

    ,setTopLevelOnly $ defRNative "enforce-seal-version" enforceVersion
     (funType tTyBool [("min-version",tTyString)] <>
      funType tTyBool [("min-version",tTyString),("max-version",tTyString)])
    "Enforce runtime seal version as greater than or equal MIN-VERSION, and less than or equal MAX-VERSION. \
    \Version values are matched numerically from the left, such that '2', '2.2', and '2.2.3' would all allow '2.2.3'. \
    \`(enforce-seal-version \"2.3\")`"

    ,defRNative "contains?" contains
    (funType tTyBool [("list",TyList a),("value",a)] <>
     funType tTyBool [("object",tTyObject (mkSchemaVar "o")),("key",a)] <>
     funType tTyBool [("string",tTyString),("value",tTyString)])
    "Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry. \
    \`(contains? [1 2 3] 2)` `(contains? { :name \"Ted\", :age 72 } :name)` `(contains? \"foobar\" \"foo\")`"

    ,defNative "constantly" constantly
     (funType a [("value",a),("ignore1",b)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c),("ignore3",d)])
     "Lazily ignore arguments IGNORE* and return VALUE. `(filter (constantly true) [1 2 3])`"
    ,defRNative "identity" identity (funType a [("value",a)])
     "Return provided value. `(map (identity) [1 2 3])`"

     ,defRNative "str-to-int" strToInt
     (funType tTyInteger [("str-val", tTyString)] <>
      funType tTyInteger [("base", tTyInteger), ("str-val", tTyString)])
     "Compute the integer value of STR-VAL in base 10, or in BASE if specified. STR-VAL must be <= 128 \
     \chars in length and BASE must be between 2 and 16. `(str-to-int 16 \"123456\")` `(str-to-int \"abcdef123456\")`"
    ,hashDef
    ])
    where b = mkTyVar "b" []
          c = mkTyVar "c" []
          d = mkTyVar "d" []
          row = mkSchemaVar "row"
          -- tyEvent = mkSchemaVar "event"
          -- rowTy = TySchema TyObject row
          -- eventTy = TySchema TyEvent obj
          yieldv = TySchema TyObject (mkSchemaVar "y")
          obj = tTyObject (mkSchemaVar "o")
          listStringA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString]
          takeDrop = funType listStringA [("count",tTyInteger),("list",listStringA)] <>
                     funType obj [("keys",TyList tTyString),("object",obj)]
          lam x y = TyFun $ funType' y [("x",x)]
          lam2 x y z = TyFun $ funType' z [("x",x),("y",y)]

a :: Type n
a = mkTyVar "a" []

map' :: NativeFun e
map' i as@[app@TApp {},l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls _ _ -> (\b -> TList b TyAny def) <$> forM ls (apply' app . pure)
           t -> evalError' i $ "map: expecting list: " ++ abbrev t
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList as TyAny (_faInfo i) -- TODO, could set type here

makeArray :: RNativeFun e
makeArray i [value,TLitInteger len] = case typeof value of
  Right ty -> return $ toTList ty def $ replicate (fromIntegral len) value
  Left ty -> evalError' i $ "make-array: invalid value type: " ++ show ty
makeArray i as = argsError i as

reverse' :: RNativeFun e
reverse' _ [l@TList{}] = return $ over tList reverse l
reverse' i as = argsError i as

empty' :: RNativeFun e
empty' _i [TList ls _ _] = return $ toTerm $ length ls <= 0
empty' _i [TObject ps _ _] = return $ toTerm $ length ps <= 0
empty' _i [TLitString s] = return $ toTerm $ (length $ unpack s) <= 0
empty' i as = argsError i as


reduce' :: NativeFun e
reduce' i as@[app@TApp {},initv,l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls _ _ -> reduce initv >>= \initv' ->
                         foldM (\r a' -> apply' app [r,a']) initv' ls
           t -> evalError' i $ "fold: expecting list: " ++ abbrev t
reduce' i as = argsError' i as


filter' :: NativeFun e
filter' i as@[app@TApp {},l] = gasUnreduced i as $ reduce l >>= \l' -> case l' of
           TList ls lt _ -> ((\b -> TList b lt def) . concat) <$>
                         forM ls (\a' -> do
                           t <- apply' app [a']
                           case t of
                             (TLiteral (LBool True) _) -> return [a']
                             _ -> return []) -- hmm, too permissive here, select is stricter
           t -> evalError' i $ "filter: expecting list: " ++ abbrev t
filter' i as = argsError' i as

count' :: RNativeFun e
count' _ [TList ls _ _] = return $ toTerm (length ls)
count' _ [TLitString s] = return $ toTerm (T.length s)
count' _ [TObject ps _ _] = return $ toTerm (length ps)
count' i as = argsError i as

take' :: RNativeFun e
take' _ [TLitInteger c,TList l t _] = return $ TList (tord take c l) t def
take' _ [TLitInteger c,TLitString l] = return $ toTerm $ pack $ tord take c (unpack l)
take' _ [l@TList {},TObject {..}] =
  return $ toTObject _tObjectType def $ (`filter` _tObject) $ \(k,_) -> searchTermList k (_tList l)

take' i as = argsError i as

drop' :: RNativeFun e
drop' _ [TLitInteger c,TList l t _] = return $ TList (tord drop c l) t def
drop' _ [TLitInteger c,TLitString l] = return $ toTerm $ pack $ tord drop c (unpack l)
drop' _ [l@TList {},TObject {..}] =
  return $ toTObject _tObjectType def $ (`filter` _tObject) $ \(k,_) -> not $ searchTermList k (_tList l)
drop' i as = argsError i as

tord :: (Int -> [a] -> [a]) -> Integer -> [a] -> [a]
tord f c l | c >= 0 = f (fromIntegral c) l
           | otherwise = reverse $ f (fromIntegral (negate c)) (reverse l)

get' :: RNativeFun e
get' _ [TList ls _ _, li@(TLitInteger idx)] =
  case ls `atMay` fromIntegral idx of
    Just t -> return t
    Nothing ->
      evalError (_tInfo li) $
      "get: bad index " ++ show idx ++ ", length " ++ show (length ls)
get' _ [TList ls _ _, (TLitInteger idx), defValue] =
  case ls `atMay` fromIntegral idx of
    Just t -> return t
    Nothing -> return defValue
get' _ [TObject ls _ _, idx] = lookupObj idx ls
get' _ [TObject ls _ _, idx, defValue] =
  case lookup (unsetInfo idx) (map (first unsetInfo) ls) of
    Just v -> return v
    Nothing -> return defValue
get' i as = argsError i as


lookupObj :: (Eq n, Show n) => Term n -> [(Term n, Term n)] -> Eval m (Term n)
lookupObj idx ls = case lookup (unsetInfo idx) (map (first unsetInfo) ls) of
  Just v -> return v
  Nothing -> evalError (_tInfo idx) $ "get: key not found: " ++ show idx


updateObj :: (Eq n) => [(Term n, Term n)]-> [(Term n, Term n)] -> [(Term n, Term n)]
updateObj _ [] = []
updateObj [] ps = ps
updateObj [p] ps = case elemIndex ((unsetInfo . fst) p) (map (unsetInfo . fst) ps) of
    Just n -> take n ps ++ [p] ++ drop (n + 1) ps
    Nothing -> ps ++ [p]
updateObj (p:px) ps = updateObj px $ updateObj [p] ps

filterObj :: (Eq n) => [Term n] -> [(Term n, Term n)] -> [(Term n, Term n)]
filterObj _ [] = []
filterObj [] ps = ps
filterObj [key] ps = filter (\(k, _) -> unsetInfo key /= unsetInfo k) ps
filterObj (k:ks) ps = filterObj ks $ filterObj [k] ps

updateList :: (Eq n, Show n) => [(Term n, Term n)] -> [Term n] -> Maybe [Term n]
updateList [] ls = Just ls
updateList [(TLitInteger idx, e)] ls =
  let n = fromIntegral idx
   in if length ls >= n
        then Just $ take n ls ++ [e] ++ drop (n + 1) ls
        else Nothing
updateList (p:ps) ls = do
  ls' <- updateList [p] ls
  updateList ps ls'



-- dissoc :: RNativeFun e
-- dissoc _ [TObject ps t _, key] = return $ TObject (filterObj [key] ps) t def
-- dissoc _ [TObject ps t _, key1, key2] =
--   return $ TObject (filterObj [key1, key2] ps) t def
-- dissoc _ [TObject ps t _, key1, key2, key3] =
--   return $ TObject (filterObj [key1, key2, key3] ps) t def
-- dissoc _ [TObject ps t _, key1, key2, key3, key4] =
--   return $ TObject (filterObj [key1, key2, key3, key4] ps) t def
-- dissoc _ [TObject ps t _, key1, key2, key3, key4, key5] =
--   return $ TObject (filterObj [key1, key2, key3, key4, key5] ps) t def
-- dissoc i as = argsError i as

dissoc' :: RNativeFun e
-- dissoc' _ [l@TList {}] = return l
-- dissoc' i as@[TList ls t _, TLitInteger idx] =
--   let n = fromIntegral idx
--    in if length ls >= n
--         then return $ TList (take n ls ++ drop (n + 1) ls) t def
--         else argsError i as
-- dissoc' i (l@TList {}:x:xs) = do
--   ls <- dissoc' i [l, x]
--   dissoc' i (ls : xs)
dissoc' _ [(TObject ps t _)] = return $ TObject ps t def
dissoc' _ ((TObject ps t _):ks) = return $ TObject (filterObj ks ps) t def
dissoc' i as = argsError i as




-- assoc :: RNativeFun e
-- assoc _ [TObject ps t _, key, value] =
--   return $ TObject (updateObj [(key, value)] ps) t def
-- assoc _ [TObject ps t _, key1, value1, key2, value2] =
--   return $ TObject (updateObj [(key1, value1), (key2, value2)] ps) t def
-- assoc _ [TObject ps t _, key1, value1, key2, value2, key3, value3] =
--   return $ TObject (updateObj [(key1, value1), (key2, value2), (key3, value3)] ps) t def
-- assoc _ [TObject ps t _, key1, value1, key2, value2, key3, value3, key4, value4] =
--   return $ TObject (updateObj [(key1, value1), (key2, value2), (key3, value3), (key4, value4)] ps) t def
-- assoc _ [TObject ps t _, key1, value1, key2, value2, key3, value3, key4, value4, key5, value5] =
--   return $ TObject (updateObj [(key1, value1), (key2, value2), (key3, value3), (key4, value4), (key5, value5)] ps) t def
-- assoc i as = argsError i as

-- 支持可变参数
assoc' :: RNativeFun e
assoc' _ [l@TList {}] = return l
assoc' i as@(TList ls t _:ivs) =
  case updateList (buildKVPairs ivs) ls of
    Just ls' -> return $ TList ls' t def
    Nothing -> argsError i as
assoc' _ [o@TObject {}] = return o
assoc' _ (TObject ps t _:kvs) =
  return $ TObject (updateObj (buildKVPairs kvs) ps) t def
assoc' i as = argsError i as




buildKVPairs :: (Eq n) => [Term n] -> [(Term n, Term n)]
buildKVPairs [] = []
buildKVPairs [_] = []
buildKVPairs [k, v] = [(k, v)]
buildKVPairs (k:v:kvs) = buildKVPairs [k, v] ++ buildKVPairs kvs

-- comp :: NativeFun e
-- comp i as@[appA@TApp {},appB@TApp {},v] = gasUnreduced i as $ do
--   v' <- reduce v
--   b' <- apply' appB [v']
--   apply' appA [b']
-- comp i as = argsError' i as

-- 支持可变参数
comp' :: NativeFun e
comp' i as@[] = argsError' i as
comp' i as =
    gasUnreduced i as $
    case reverse as of
        [v] -> reduce v
        (v:funs) -> do
            v' <- reduce v
            applyComp funs v'
        _ -> argsError' i as

applyComp :: [Term Ref] -> Term Name -> Eval e (Term Name)
applyComp [] _ = fail $ "applyComp failure"
applyComp [x] v = apply' x [v]
applyComp (x:xs) v = do
  v' <- apply' x [v]
  applyComp xs v'


readMsg :: RNativeFun e
readMsg i [TLitString key] = parseMsgKey i "read-msg" key
readMsg _ [] = TValue <$> view eeMsgBody <*> pure def
readMsg i as = argsError i as

-- | One-off type for 'readDecimal', not exported.
newtype ParsedDecimal = ParsedDecimal Decimal
instance FromJSON ParsedDecimal where
  parseJSON (String s) =
    ParsedDecimal <$> case AP.parseOnly (unPactParser number) s of
                        Right (LDecimal d) -> return d
                        Right (LInteger i) -> return (fromIntegral i)
                        _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing integer: " ++ show v


-- | One-off type for 'readInteger', not exported.
newtype ParsedInteger = ParsedInteger Integer
instance FromJSON ParsedInteger where
  parseJSON (String s) =
    ParsedInteger <$> case AP.parseOnly (unPactParser number) s of
                        Right (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (Number n) = return $ ParsedInteger (round n)
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

readInteger :: RNativeFun e
readInteger i [TLitString key] = do
  (ParsedInteger a') <- parseMsgKey i "read-integer" key
  return $ toTerm a'
readInteger i as = argsError i as


pactId :: RNativeFun e
pactId i [] = use evalPactExec >>= \pe -> case pe of
  Nothing -> evalError' i "seal-id: not in seal execution"
  Just PactExec{..} -> return $ toTerm _pePactId
pactId i as = argsError i as

bind :: NativeFun e
bind i as@[src,TBinding ps bd (BindSchema _) bi] = gasUnreduced i as $
  reduce src >>= bindObjectLookup >>= bindReduce ps bd bi
bind i as = argsError' i as

bindObjectLookup :: Term Name -> Eval e (Text -> Maybe (Term Ref))
bindObjectLookup TObject {..} = do
  !m <- fmap M.fromList $ forM _tObject $ \(k,v) -> case k of
    TLitString k' -> return (k',liftTerm v)
    tk -> evalError _tInfo $ "Bad object (non-string key) in bind: " ++ show tk
  return (\s -> M.lookup s m)
bindObjectLookup t = evalError (_tInfo t) $ "bind: expected object: " ++ show t

typeof'' :: RNativeFun e
typeof'' _ [t] = return $ tStr $ typeof' t
typeof'' i as = argsError i as

listModules :: RNativeFun e
listModules _ _ = do
  mods <- view $ eeRefStore.rsModules
  return $ toTermList tTyString $ map asString $ M.keys mods

unsetInfo :: Term a -> Term a
unsetInfo a' = set tInfo def a'
{-# INLINE unsetInfo #-}

yield :: RNativeFun e
yield i [t@TObject {}] = do
  eym <- use evalPactExec
  case eym of
    Nothing -> evalError' i "Yield not in defpact context"
    Just {} -> do
      (evalPactExec . _Just . peYield) .= Just t
      return t
yield i as = argsError i as

resume :: NativeFun e
resume i as@[TBinding ps bd (BindSchema _) bi] = gasUnreduced i as $ do
  rm <- asks $ firstOf $ eePactStep . _Just . psResume . _Just
  case rm of
    Nothing -> evalError' i "Resume: no yielded value in context"
    Just rval -> bindObjectLookup rval >>= bindReduce ps bd bi
resume i as = argsError' i as

where' :: NativeFun e
where' i as@[k',app@TApp{},r'] = gasUnreduced i as $ ((,) <$> reduce k' <*> reduce r') >>= \kr -> case kr of
  (k,r@TObject {}) -> lookupObj k (_tObject r) >>= \v -> apply' app [v]
  _ -> argsError' i as
where' i as = argsError' i as


sort' :: RNativeFun e
sort' _ [TList{..}] = case nub (map typeof _tList) of
  [ty] -> case ty of
    Right rty@(TyPrim pty) -> case pty of
      TyValue -> badTy (show ty)
      TyKeySet -> badTy (show ty)
      _ -> do
        sl <- forM _tList $ \e -> case firstOf tLiteral e of
          Nothing -> evalError _tInfo $ "Unexpected type error, expected literal: " ++ show e
          Just lit -> return (lit,e)
        return $ TList (map snd $ sortBy (compare `on` fst) sl) rty def
    _ -> badTy (show ty)
  ts -> evalError _tInfo $ "sort: non-uniform list: " ++ show ts
  where badTy s = evalError _tInfo $ "sort: bad list type: " ++ s
sort' _ [fields@TList{},l@TList{}]
  | null (_tList fields) = evalError (_tInfo fields) "Empty fields list"
  | otherwise = do
      sortPairs <- forM (_tList l) $ \el -> case firstOf tObject el of
        Nothing -> evalError (_tInfo l) $ "Non-object found: " ++ show el
        Just o -> fmap ((,el) . reverse) $ (\f -> foldM f [] (_tList fields)) $ \lits fld -> do
          v <- lookupObj fld o
          case firstOf tLiteral v of
            Nothing -> evalError (_tInfo l) $ "Non-literal found at field " ++ show fld ++ ": " ++ show el
            Just lit -> return (lit:lits)
      return $ TList (map snd $ sortBy (compare `on` fst) sortPairs) (_tListType l) def

sort' i as = argsError i as


enforceVersion :: RNativeFun e
enforceVersion i as = case as of
  [TLitString minVersion] -> doMin minVersion >> return (toTerm True)
  [TLitString minVersion,TLitString maxVersion] ->
    doMin minVersion >> doMax maxVersion >> return (toTerm True)
  _ -> argsError i as
  where
    doMin = doMatch "minimum" (>) (<)
    doMax = doMatch "maximum" (<) (>)
    doMatch msg failCmp succCmp fullV =
      foldM_ matchPart False $ zip (T.splitOn "." pactVersion) (T.splitOn "." fullV)
      where
        parseNum orgV s = case AP.parseOnly (AP.many1 AP.digit) s of
          Left _ -> evalError' i $ "Invalid version component: " ++ show (orgV,s)
          Right v -> return v
        matchPart True _ = return True
        matchPart _ (pv,mv)  = do
          pv' <- parseNum pactVersion pv
          mv' <- parseNum fullV mv
          when (mv' `failCmp` pv') $ evalError' i $
            "Invalid seal version " ++ show pactVersion ++ ", " ++ msg ++ " allowed: " ++ show fullV
          return (mv' `succCmp` pv')


contains :: RNativeFun e
contains _i [TList {..}, val] = return $ toTerm $ searchTermList val _tList
contains _i [TObject {..}, k] = return $ toTerm $ foldl search False _tObject
  where
    search True _ = True
    search _ (t, _) = t `termEq` k
contains _i [TLitString t, TLitString s] = return $ toTerm $ T.isInfixOf s t
contains i as = argsError i as


searchTermList :: (Foldable t, Eq n) => Term n -> t (Term n) -> Bool
searchTermList val = foldl search False
  where search True _ = True
        search _ t = t `termEq` val


constantly :: NativeFun e
constantly i (v:_) = gasUnreduced i [v] $ reduce v
constantly i as = argsError' i as

identity :: RNativeFun e
identity _ [a'] = return a'
identity i as = argsError i as

strToInt :: RNativeFun e
strToInt i as =
  case as of
    [TLitString s] -> go 10 s
    [TLitInteger base, TLitString s] -> go base s
    _ -> argsError i as
  where
    go base' txt =
      if T.all isHexDigit txt
      then
        if T.length txt <= 128
        then case baseStrToInt base' txt of
          Left _ -> argsError i as
          Right n -> return (toTerm n)
        else evalError' i $ "Invalid input: unsupported string length: " ++ (unpack txt)
      else evalError' i $ "Invalid input: supplied string is not hex: " ++ (unpack txt)

txHash :: RNativeFun e
txHash _ [] = (tStr . txHash1) <$> view eeHash
txHash i as = argsError i as

txHash1 :: Hash -> Text
txHash1 (Hash h) = pack $ BSC.unpack h

msgSender :: RNativeFun e
msgSender _ [] = view eeMsgSigs >>= \ss -> return $ toTerm $ (decodeUtf8 . _pubKey) $ head (S.toList ss)
msgSender i as = argsError i as

msgValue :: RNativeFun e
msgValue _ [] = (tStr . asString) <$> view eeHash
msgValue i as = argsError i as

-- | Change of base for Text-based representations of integrals. Only bases
-- 2 through 16 are supported, for non-empty text of length <= 128
--
-- e.g.
--   -- hexadecimal to decimal
--   baseStrToInt 10 "abcdef123456" = 188900967593046
--
baseStrToInt :: Integer -> Text -> Either Text Integer
baseStrToInt base t =
  if base <= 1 || base > 16
  then Left $ "baseStrToInt - unsupported base: " `T.append` asString base
  else
    if T.null t
    then Left $ "baseStringToInt - empty text: " `T.append` asString t
    else Right $ T.foldl' go 0 t
  where
    go :: Integer -> Char -> Integer
    go acc w = base * acc + (fromIntegral . digitToInt $ w)
{-# INLINE baseStrToInt #-}

