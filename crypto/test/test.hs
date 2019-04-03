import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Seal.Crypto.Bi
import qualified Test.Seal.Crypto.Json
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Seal.Crypto.Bi.tests
        , Test.Seal.Crypto.Json.tests
        ]
