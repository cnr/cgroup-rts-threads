module Main (
  main,
) where

import qualified System.CGroup.CPUSpec as CPUSpec
import qualified System.CGroup.TypesSpec as TypesSpec
import Test.Sandwich (CoreSpec, defaultOptions, runSandwich)

main :: IO ()
main = runSandwich defaultOptions tests

tests :: CoreSpec
tests = do
  CPUSpec.tests
  TypesSpec.tests
