module Main (
  main,
) where

import qualified System.CGroup.CPUSpec as CPUSpec
import qualified System.CGroup.ControllerSpec as ControllerSpec
import Test.Hspec.Core.Runner (hspec)
import Test.Hspec.Core.Spec (Spec)

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  CPUSpec.tests
  ControllerSpec.tests
