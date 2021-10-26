module Main (
  main,
) where

import System.CGroup.CPUSpec qualified as CPUSpec
import System.CGroup.TypesSpec qualified as TypesSpec
import Test.Sandwich (CoreSpec, defaultOptions, runSandwich)

main :: IO ()
main = runSandwich defaultOptions tests

tests :: CoreSpec
tests = do
  CPUSpec.tests
  TypesSpec.tests
