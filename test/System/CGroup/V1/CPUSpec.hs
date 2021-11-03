module System.CGroup.V1.CPUSpec (
  tests,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Ratio ((%))
import Path.IO (resolveDir')
import System.CGroup.V1.CPU
import System.CGroup.V1.Controller (Controller (..))
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

tests :: Spec
tests = do
  describe "getCPUQuota" $ do
    it "should return CPUQuota when there is a quota" $ do
      controller <- resolveDir' "test/System/CGroup/V1/testdata-cpu/quota"
      quota <- liftIO $ getCPUQuota (Controller controller)
      quota `shouldBe` CPUQuota (1 % 2)

    it "should return NoQuota when there is no quota" $ do
      controller <- resolveDir' "test/System/CGroup/V1/testdata-cpu/noquota"
      quota <- liftIO $ getCPUQuota (Controller controller)
      quota `shouldBe` NoQuota
