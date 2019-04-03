import           Prelude (IO)

import           Test.Hspec (hspec)

import           Spec (spec)
import qualified Test.Seal.Infra.Bi
import qualified Test.Seal.Infra.Json
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Seal.Infra.Bi.tests
        , Test.Seal.Infra.Json.tests
        ]
