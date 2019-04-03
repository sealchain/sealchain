{-# LANGUAGE RecordWildCards #-}

module Seal.Core.Common.Cmd 
( Cmd (..)
) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, string, (%))

import           Seal.Binary.Class (Bi (..)) -- |, cborError)

newtype Cmd = Cmd
    { getCmd :: String
    } deriving (Show, Ord, Eq, Hashable, NFData)

instance Buildable Cmd where
    build Cmd {..} =
        bprint 
            ("Cmd: " %string)
            getCmd 

-- instance Bi Cmd where
--   encode (Cmd cmd) = 
--         encode byteString
--     where 
--         byteString :: ByteString 
--         byteString = encodeUtf8 cmd

--   decode = do
--     let utf8Err = "Invalid utf-8 representation of commands"
--     byteString <- decode
--     case (decodeUtf8' byteString) of 
--         Left _    -> cborError utf8Err
--         Right cmd -> pure . Cmd $ toString cmd

instance Bi Cmd where
    encode = encode . getCmd
    decode = Cmd <$> decode

deriveJSON defaultOptions ''Cmd
deriveSafeCopySimple 0 'base ''Cmd