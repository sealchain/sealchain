module Test.Seal.Chain.Delegation.Example
       ( exampleLightDlgIndices
       , exampleProxySKBlockInfo
       , exampleUndo
       , staticHeavyDlgIndexes
       , staticProxySKHeavys
       ) where

import           Universum

import qualified Data.HashSet as HS
import           Data.List (zipWith4, (!!))

import           Seal.Chain.Delegation (DlgUndo (..), HeavyDlgIndex (..),
                     LightDlgIndices (..), ProxySKBlockInfo, ProxySKHeavy)
import           Seal.Core (EpochIndex (..))
import           Seal.Crypto (ProtocolMagic (..), ProtocolMagicId (..),
                     RequiresNetworkMagic (..), safeCreatePsk)

import           Test.Seal.Core.ExampleHelpers (examplePublicKey,
                     examplePublicKeys, exampleStakeholderId,
                     staticSafeSigners)


staticHeavyDlgIndexes :: [HeavyDlgIndex]
staticHeavyDlgIndexes = map (HeavyDlgIndex . EpochIndex) [5,1,3,27,99,247]

staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map mkPm [0..5]
  where
    mkPm :: Int32 -> ProtocolMagic
    mkPm x = ProtocolMagic { getProtocolMagicId = ProtocolMagicId x
                           , getRequiresNetworkMagic = RequiresNoMagic
                           }

staticProxySKHeavys :: [ProxySKHeavy]
staticProxySKHeavys = zipWith4 safeCreatePsk
                               staticProtocolMagics staticSafeSigners
                               (examplePublicKeys 1 6) staticHeavyDlgIndexes

exampleProxySKBlockInfo :: ProxySKBlockInfo
exampleProxySKBlockInfo = Just (staticProxySKHeavys !! 0, examplePublicKey)

exampleLightDlgIndices :: LightDlgIndices
exampleLightDlgIndices = LightDlgIndices (EpochIndex 7, EpochIndex 88)

exampleUndo :: DlgUndo
exampleUndo = DlgUndo
  { duPsks = staticProxySKHeavys
  , duPrevEpochPosted = HS.singleton exampleStakeholderId
  }
