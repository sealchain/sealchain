{-# LANGUAGE CPP #-}
-- |
-- Module     : Seal.Contract.Types.Version
-- Copyright  : (C) 2017 Stuart Popejoy
-- License    : BSD-style (see the file LICENSE)
-- Maintainer : Stuart Popejoy <stuart@kadena.io>
--
-- Use CPP to retrieve cabal library version.
--
module Seal.Contract.Types.Version (pactVersion) where

import Data.Text (Text, pack)

pactVersion :: Text
pactVersion = pack "1.0.0"
