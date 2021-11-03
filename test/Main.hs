module Main (
  main,
) where

import qualified System.CGroup.V1.CPUSpec as V1.CPUSpec
import qualified System.CGroup.V1.ControllerSpec as V1.ControllerSpec
import qualified System.CGroup.V2.CGroupSpec as V2.CGroupSpec
import qualified System.CGroup.V2.CPUSpec as V2.CPUSpec
import Test.Hspec.Core.Runner (hspec)
import Test.Hspec.Core.Spec (Spec)

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  V1.CPUSpec.tests
  V1.ControllerSpec.tests
  V2.CGroupSpec.tests
  V2.CPUSpec.tests
