import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Seal.Binary.BiSerialize
import qualified Test.Seal.Binary.BiSizeBounds
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Seal.Binary.BiSerialize.tests
        , Test.Seal.Binary.BiSizeBounds.tests
        ]
