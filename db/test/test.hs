import           Universum

import qualified Test.Seal.DB.Epoch.Index
import qualified Test.Seal.DB.Functions
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = runTests [Test.Seal.DB.Epoch.Index.tests, Test.Seal.DB.Functions.tests]
