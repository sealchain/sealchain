{-# LANGUAGE OverloadedStrings #-}

module Seal.Mpt.MerklePatricia.Utils (
  assertEitherSuccess
  --, assertSuccess
  --, byteString2TermNibbleString
 -- , string2TermNibbleString
  , termNibbleString2String
  , transKV
  , strToNibbleString
  , bytesToNibbleString
  ) where

import           Universum    hiding(fail)
--import           Control.Monad.Fail


import qualified Data.NibbleString                            as N
import qualified Data.ByteString                              as B
import           Data.ByteString.Char8 as BC hiding(map)

import Data.Word ( Word8 ) 
import Unsafe.Coerce ( unsafeCoerce ) 
import Seal.Mpt.MerklePatricia.NodeData

assertEitherSuccess :: Monad m => Either Text NodeData -> m NodeData
assertEitherSuccess (Right a) = return a
assertEitherSuccess _ = return EmptyNodeData


{-
assertSuccess :: MonadFail m => Either Text a -> m a
assertSuccess (Right a) = return a
assertSuccess (Left msg) = fail $ toString msg
-}

--byteString2TermNibbleString::B.ByteString -> N.NibbleString
--byteString2TermNibbleString bs = snd . string2TermNibbleString $ BC.unpack bs


transKV :: (N.NibbleString, B.ByteString) -> (B.ByteString, B.ByteString)
transKV kv =  (BC.tail $ termNibbleString2String True $ fst kv,snd kv)
          

strToNibbleString :: String -> N.NibbleString
strToNibbleString str = N.pack $ (N.byte2Nibbles =<<) $ strToWord8 str


bytesToNibbleString :: B.ByteString -> N.NibbleString
bytesToNibbleString bytes = N.pack $ (N.byte2Nibbles =<<)  $ B.unpack bytes

toWord8 :: Char -> Word8
toWord8 = unsafeCoerce

strToWord8 :: String -> [Word8]
strToWord8 = map toWord8




