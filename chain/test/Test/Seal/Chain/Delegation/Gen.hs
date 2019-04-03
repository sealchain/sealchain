module Test.Seal.Chain.Delegation.Gen
       ( genDlgPayload
       , genHeavyDlgIndex
       , genLightDlgIndices
       , genProxySKBlockInfo
       , genProxySKHeavy
       , genUndo
       ) where

import           Universum

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Seal.Chain.Delegation (DlgPayload (..), DlgUndo (..),
                     HeavyDlgIndex (..), LightDlgIndices (..),
                     ProxySKBlockInfo, ProxySKHeavy)
import           Seal.Crypto (ProtocolMagic, safeCreatePsk)

import           Test.Seal.Core.Gen (genEpochIndex, genStakeholderId)
import           Test.Seal.Crypto.Gen (genPublicKey, genSafeSigner)
import           Test.Seal.Util.Gen (genHashSet)


genDlgPayload :: ProtocolMagic -> Gen DlgPayload
genDlgPayload pm =
    UnsafeDlgPayload <$> Gen.list (Range.linear 0 5) (genProxySKHeavy pm)

genHeavyDlgIndex :: Gen HeavyDlgIndex
genHeavyDlgIndex = HeavyDlgIndex <$> genEpochIndex

genLightDlgIndices :: Gen LightDlgIndices
genLightDlgIndices =
    LightDlgIndices <$> ((,) <$> genEpochIndex <*> genEpochIndex)

genProxySKBlockInfo :: ProtocolMagic -> Gen ProxySKBlockInfo
genProxySKBlockInfo pm = Gen.maybe $ do
    pSKHeavy <- genProxySKHeavy pm
    pubKey <- genPublicKey
    pure (pSKHeavy,pubKey)

genProxySKHeavy :: ProtocolMagic -> Gen ProxySKHeavy
genProxySKHeavy pm =
    safeCreatePsk pm
        <$> genSafeSigner
        <*> genPublicKey
        <*> genHeavyDlgIndex

genUndo :: ProtocolMagic -> Gen DlgUndo
genUndo pm =
  DlgUndo
    <$> Gen.list (Range.linear 1 10) (genProxySKHeavy pm)
    <*> genHashSet genStakeholderId
