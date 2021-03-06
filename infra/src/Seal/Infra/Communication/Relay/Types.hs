{-# LANGUAGE GADTs #-}

module Seal.Infra.Communication.Relay.Types
       ( RelayError (..)
       , PropagationMsg (..)
       ) where

import           Prelude (Show (..))
import           Universum hiding (Show)

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable
import           Node (Message)

import           Seal.Binary.Class (Bi)
import           Seal.Infra.Communication.Types.Protocol (Msg)
import           Seal.Infra.Communication.Types.Relay (DataMsg, InvOrData,
                     ReqOrRes)

data RelayError = UnexpectedInv
                | UnexpectedData
  deriving (Generic, Show)

instance Exception RelayError

data PropagationMsg where
    InvReqDataPM ::
        ( Message (InvOrData key contents)
        , Bi (InvOrData key contents)
        , Buildable key
        , Eq key
        , Message (ReqOrRes key)
        , Bi (ReqOrRes key))
        => !Msg
        -> !key
        -> !contents
        -> PropagationMsg
    DataOnlyPM ::
        ( Message (DataMsg contents)
        , Bi (DataMsg contents)
        , Buildable contents)
        => !Msg
        -> !contents
        -> PropagationMsg

instance Buildable PropagationMsg where
    build (InvReqDataPM _ key _) =
        bprint ("<data for key "%build%">") key
    build (DataOnlyPM _ conts) =
        Buildable.build conts
