module Bench.Configuration
    ( benchProtocolConstants
    , benchProtocolMagic
    ) where

import           Seal.Core (ProtocolConstants (..), VssMaxTTL (..),
                     VssMinTTL (..))
import           Seal.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..))

benchProtocolConstants :: ProtocolConstants
benchProtocolConstants = ProtocolConstants
    { pcK = 2
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

benchProtocolMagic :: ProtocolMagic
benchProtocolMagic = ProtocolMagic (ProtocolMagicId 55550001) RequiresNoMagic
