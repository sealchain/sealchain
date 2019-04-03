module Test.Seal.Core.SafeCopy where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Seal.Core.ExampleHelpers (exampleAddress, exampleAddress',
                     exampleAddress'1, exampleAddress'2,
                     exampleAddress'4,
                     exampleAddress1, exampleAddress2,
                     exampleAddress4)
import           Test.Seal.Util.Golden (discoverGolden, goldenTestSafeCopyDec)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `Address` `SafeCopy` format, the `RequiresNetworkMagic` field defaults to
-- `RequiresNoMagic`.

golden_Address0 :: Property
golden_Address0 =
    goldenTestSafeCopyDec
        exampleAddress
        "test/golden/safecopy/Address0_Legacy_NoNetworkMagic"

golden_Address1 :: Property
golden_Address1 =
    goldenTestSafeCopyDec
        exampleAddress1
        "test/golden/safecopy/Address1_Legacy_NoNetworkMagic"

golden_Address2 :: Property
golden_Address2 =
    goldenTestSafeCopyDec
        exampleAddress2
        "test/golden/safecopy/Address2_Legacy_NoNetworkMagic"

golden_Address4 :: Property
golden_Address4 =
    goldenTestSafeCopyDec
        exampleAddress4
        "test/golden/safecopy/Address4_Legacy_NoNetworkMagic"

--------------------------------------------------------------------------------
-- Address'
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `Address'` `SafeCopy` format, the `RequiresNetworkMagic` field defaults to
-- `RequiresNoMagic`.

golden_Address'0 :: Property
golden_Address'0 =
    goldenTestSafeCopyDec
        exampleAddress'
        "test/golden/safecopy/Address'0_Legacy_NoNetworkMagic"

golden_Address'1 :: Property
golden_Address'1 =
    goldenTestSafeCopyDec
        exampleAddress'1
        "test/golden/safecopy/Address'1_Legacy_NoNetworkMagic"

golden_Address'2 :: Property
golden_Address'2 =
    goldenTestSafeCopyDec
        exampleAddress'2
        "test/golden/safecopy/Address'2_Legacy_NoNetworkMagic"

golden_Address'4 :: Property
golden_Address'4 =
    goldenTestSafeCopyDec
        exampleAddress'4
        "test/golden/safecopy/Address'4_Legacy_NoNetworkMagic"

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
