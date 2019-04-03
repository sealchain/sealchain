import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Seal.Configuration (defaultTestConf)
import qualified Test.Seal.Launcher.Json
import           Test.Seal.Util.Tripping (runTests)

main :: IO ()
main = do
    putText $ "default configuration: " <> show defaultTestConf
    hspec spec
    runTests [ Test.Seal.Launcher.Json.tests ]
