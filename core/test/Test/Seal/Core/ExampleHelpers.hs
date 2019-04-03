module Test.Seal.Core.ExampleHelpers
       (  -- Example data

          exampleAddrSpendingData_PubKey
        , exampleAddress
        , exampleAddress1
        , exampleAddress2
        , exampleAddress4
        , exampleAddress6
        , exampleAddress7
        , exampleAddress'
        , exampleAddress'1
        , exampleAddress'2
        , exampleAddress'4
        , exampleAddress'6
        , exampleAddress'7
        , exampleAttributes
        , exampleChainDifficulty
        , exampleEpochIndex
        , examplePublicKey
        , examplePublicKeys
        , exampleRedeemPublicKey
        , exampleSafeSigner
        , exampleSecretKey
        , exampleSecretKeys
        , exampleSharedSeed0
        , exampleSharedSeed1
        , exampleSharedSeed2
        , exampleSlotId
        , exampleSlottingData
        , exampleSlotLeaders
        , exampleStakeholderId
        , exampleStakeholderIds
        , exampleStakesList
        , exampleVssPublicKeys
        , staticSafeSigners

        -- Helpers
        , feedPM
        , feedPMWithRequiresMagic
        , feedPC
        , feedPMC
        , feedEpochSlots
        , feedPMEpochSlots
        , getText
       ) where

import           Universum

import           Data.List ((!!))
import           Data.List.NonEmpty (fromList)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Hedgehog as H

import qualified Seal.Crypto.Wallet as CC
import           Seal.Core.Attributes (Attributes, mkAttributes)
import           Seal.Core.Common (AddrCategory (..), AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), Address (..), Address',
                     BlockCount (..), ChainDifficulty (..), Coin (..),
                     SharedSeed (..),
                     SlotLeaders, StakeholderId, StakesList,
                     makeAddress, makeAddress')
import           Seal.Core.ProtocolConstants (ProtocolConstants, pcEpochSlots)
import           Seal.Core.Slotting (EpochIndex (..), EpochSlottingData (..),
                     LocalSlotIndex (..), SlotCount, SlotId (..), SlottingData,
                     createSlottingDataUnsafe)
import           Seal.Crypto (HDAddressPayload (..), ProtocolMagic (..),
                     RedeemPublicKey, RequiresNetworkMagic (..),
                     SafeSigner (..), SecretKey (..), VssPublicKey (..),
                     abstractHash, deterministicVssKeyGen,
                     redeemDeterministicKeyGen, toVssPublicKey)
import           Seal.Crypto.Signing (PublicKey (..))

import           Test.Seal.Core.Gen (genProtocolConstants)
import           Test.Seal.Crypto.Bi (getBytes)
import           Test.Seal.Crypto.Gen (genProtocolMagic, genProtocolMagicId)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic

feedPMWithRequiresMagic :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPMWithRequiresMagic genA = do
    pm <- flip ProtocolMagic RequiresMagic <$> genProtocolMagicId
    genA pm

feedPC :: (ProtocolConstants -> H.Gen a) -> H.Gen a
feedPC genA = genA =<< genProtocolConstants

feedPMC :: (ProtocolMagic -> ProtocolConstants -> H.Gen a) -> H.Gen a
feedPMC genA = do
    pm <- genProtocolMagic
    pc <- genProtocolConstants
    genA pm pc

feedEpochSlots :: (SlotCount -> H.Gen a) -> H.Gen a
feedEpochSlots genA = genA =<< pcEpochSlots <$> genProtocolConstants

feedPMEpochSlots :: (ProtocolMagic -> SlotCount -> H.Gen a) -> H.Gen a
feedPMEpochSlots genA = do
    pm <- genProtocolMagic
    epochSlots <- pcEpochSlots <$> genProtocolConstants
    genA pm epochSlots

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners!!offset

exampleStakeholderId :: StakeholderId
exampleStakeholderId = abstractHash examplePublicKey :: StakeholderId

exampleStakeholderIds :: Int -> Int -> [StakeholderId]
exampleStakeholderIds offset l = map abstractHash $ examplePublicKeys offset l

exampleVssPublicKeys :: Int -> Int -> [VssPublicKey]
exampleVssPublicKeys offset count = map (toKey . (*offset)) [0..count]
    where
        toKey start = toVssPublicKey . deterministicVssKeyGen $ (getBytes start 32)

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePublicKey

examplePublicKey :: PublicKey
examplePublicKey = pk
  where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right pk = PublicKey <$> CC.xpub (getBytes start 64)
                   in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey = fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = (exampleSecretKeys 10 2) !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right sk = SecretKey <$> CC.xprv (getBytes start 128)
                   in sk

exampleStakesList :: StakesList
exampleStakesList = zip sis coins
  where
    sis   = map abstractHash (examplePublicKeys 15 3)
    coins = map Coin [79, 44, 9999999]

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = map abstractHash (fromList (examplePublicKeys 16 3))

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map FakeSigner (exampleSecretKeys 1 6)

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
staticText :: Text
staticText
    = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

getText :: Int -> Int -> Text
getText offset len = T.take len $ T.drop offset staticText

exampleAddress :: Address
exampleAddress = makeAddress exampleAddrSpendingData_PubKey attrs
  where
    attrs = AddrAttributes hap BootstrapEraDistr $ Just ACMobile
    hap = Just (HDAddressPayload (getBytes 32 32))

exampleAddress1 :: Address
exampleAddress1 = makeAddress easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 24 1
    attrs = AddrAttributes hap BootstrapEraDistr $ Just ACMobile
    hap = Nothing

exampleAddress2 :: Address
exampleAddress2 = makeAddress easd attrs
  where
    easd = RedeemASD exampleRedeemPublicKey
    attrs = AddrAttributes hap asd $ Just ACMobile
    hap = Just (HDAddressPayload (getBytes 15 32))
    asd = SingleKeyDistr exampleStakeholderId

exampleAddress4 :: Address
exampleAddress4 = makeAddress easd attrs
  where
    easd = UnknownASD 7 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) $ Just ACMobile
    [sId] = exampleStakeholderIds 7 1

exampleAddress6 :: Address
exampleAddress6 = makeAddress easd attrs
  where
    easd = UnknownASD 200 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) (Just (ACUnknown 3))
    [sId] = exampleStakeholderIds 10 1

exampleAddress7 :: Address
exampleAddress7 = makeAddress easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 16 1
    attrs = AddrAttributes hap BootstrapEraDistr (Just (ACUnknown 3))
    hap = Nothing

exampleAddress' :: Address'
exampleAddress' = makeAddress' exampleAddrSpendingData_PubKey attrs
  where
    attrs = AddrAttributes hap BootstrapEraDistr $ Just ACMobile
    hap = Just (HDAddressPayload (getBytes 32 32))

exampleAddress'1 :: Address'
exampleAddress'1 = makeAddress' easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 24 1
    attrs = AddrAttributes hap BootstrapEraDistr $ Just ACMobile
    hap = Nothing

exampleAddress'2 :: Address'
exampleAddress'2 = makeAddress' easd attrs
  where
    easd = RedeemASD exampleRedeemPublicKey
    attrs = AddrAttributes hap asd $ Just ACMobile
    hap = Just (HDAddressPayload (getBytes 15 32))
    asd = SingleKeyDistr exampleStakeholderId

exampleAddress'4 :: Address'
exampleAddress'4 = makeAddress' easd attrs
  where
    easd = UnknownASD 7 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) $ Just ACMobile
    [sId] = exampleStakeholderIds 7 1

exampleAddress'6 :: Address'
exampleAddress'6 = makeAddress' easd attrs
  where
    easd = UnknownASD 200 "test value"
    attrs = AddrAttributes Nothing (SingleKeyDistr sId) (Just (ACUnknown 3))
    [sId] = exampleStakeholderIds 10 1

exampleAddress'7 :: Address'
exampleAddress'7 = makeAddress' easd attrs
  where
    easd = PubKeyASD pk
    [pk] = examplePublicKeys 16 1
    attrs = AddrAttributes hap BootstrapEraDistr (Just (ACUnknown 3))
    hap = Nothing

exampleSharedSeed0 :: SharedSeed
exampleSharedSeed0 = SharedSeed (getBytes 8 32)

exampleSharedSeed1 :: SharedSeed
exampleSharedSeed1 = SharedSeed (getBytes 16 32)

exampleSharedSeed2 :: SharedSeed
exampleSharedSeed2 = SharedSeed (getBytes 24 32)

exampleSlottingData :: SlottingData
exampleSlottingData =
  createSlottingDataUnsafe
    $   M.fromList
    $   (,)
    <$> [0 .. 9]
    <*> pure exampleEpochSlottingData

exampleEpochSlottingData :: EpochSlottingData
exampleEpochSlottingData = EpochSlottingData
    { esdSlotDuration = 100
    , esdStartDiff = 100
    }
