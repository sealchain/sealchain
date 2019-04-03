import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Seal.Core.Bi
import qualified Test.Seal.Core.Json
import qualified Test.Seal.Core.SafeCopy
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Seal.Core.Bi.tests
        , Test.Seal.Core.Json.tests
        , Test.Seal.Core.SafeCopy.tests
        ]
