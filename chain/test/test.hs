import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Seal.Chain.Block.Bi
import qualified Test.Seal.Chain.Delegation.Bi
import qualified Test.Seal.Chain.Genesis.Json
import qualified Test.Seal.Chain.Ssc.Bi
import qualified Test.Seal.Chain.Ssc.Json
import qualified Test.Seal.Chain.Txp.Bi
import qualified Test.Seal.Chain.Txp.Json
import qualified Test.Seal.Chain.Update.Bi
import qualified Test.Seal.Chain.Update.Json
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Seal.Chain.Block.Bi.tests
        , Test.Seal.Chain.Delegation.Bi.tests
        , Test.Seal.Chain.Genesis.Json.tests
        , Test.Seal.Chain.Ssc.Bi.tests
        , Test.Seal.Chain.Ssc.Json.tests
        , Test.Seal.Chain.Txp.Bi.tests
        , Test.Seal.Chain.Txp.Json.tests
        , Test.Seal.Chain.Update.Bi.tests
        , Test.Seal.Chain.Update.Json.tests
        ]
