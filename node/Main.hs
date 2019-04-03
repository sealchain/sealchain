module Main
    ( main
    ) where

import           Universum

import           Seal.Client.CLI (SimpleNodeArgs (..), getSimpleNodeOptions,
                     loggingParams)
import           Seal.Launcher (actionWithCoreNode, launchNode)
import           Seal.Util.CompileInfo (withCompileInfo)


main :: IO ()
main = withCompileInfo $ do
    SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
    let lArgs = loggingParams "node" cArgs
    launchNode nArgs cArgs lArgs actionWithCoreNode
