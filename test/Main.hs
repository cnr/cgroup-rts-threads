module Main (
  main,
) where

import qualified System.CGroup.CPUSpec as CPUSpec
import qualified System.CGroup.TypesSpec as TypesSpec
import Test.Hspec.Core.Runner (hspec)
import Test.Hspec.Core.Spec (Spec)

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  CPUSpec.tests
  TypesSpec.tests
