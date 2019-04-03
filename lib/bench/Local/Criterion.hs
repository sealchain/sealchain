module Main
  ( main
  ) where

import           Universum

import           System.IO (hSetEncoding, stdout, utf8)

import qualified Bench.Seal.Criterion.FollowTheSatoshiBench as FTS
import qualified Bench.Seal.Criterion.TxSigningBench as TS
import qualified Bench.Seal.Diffusion.BlockDownload as BD

main :: IO ()
main = do
    hSetEncoding stdout utf8
    FTS.runBenchmark
    TS.runBenchmark
    BD.runBenchmark
