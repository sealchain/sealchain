-- | Dummy values used in tests (replacing `configuration.yaml`)

module Test.Seal.Crypto.Dummy
       ( dummyProtocolMagic
       , dummyProtocolMagicId
       , dummyRequiresNetworkMagic
       ) where

import           Seal.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..))

dummyProtocolMagic :: ProtocolMagic
dummyProtocolMagic = ProtocolMagic dummyProtocolMagicId RequiresNoMagic

dummyProtocolMagicId :: ProtocolMagicId
dummyProtocolMagicId = ProtocolMagicId 55550001

dummyRequiresNetworkMagic :: RequiresNetworkMagic
dummyRequiresNetworkMagic = RequiresNoMagic
