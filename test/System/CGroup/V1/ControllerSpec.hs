module System.CGroup.V1.ControllerSpec (
  tests,
) where

import Control.Monad.IO.Class (liftIO)
import Path
import Path.IO (resolveFile')
import System.CGroup.V1.Controller
import System.Info (os)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

-- This test won't work on Windows, because paths starting with `/` are invalid
tests :: Spec
tests = exceptOnWindows $ do
  describe "resolveGroupController" $ do
    it "should work on a real world example" $ do
      cgroup <- resolveFile' "test/System/CGroup/V1/testdata-controller/realworld/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/V1/testdata-controller/realworld/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should resolve a direct mount root" $ do
      cgroup <- resolveFile' "test/System/CGroup/V1/testdata-controller/direct/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/V1/testdata-controller/direct/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should resolve subdirectories of a mount root" $ do
      cgroup <- resolveFile' "test/System/CGroup/V1/testdata-controller/indirect/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/V1/testdata-controller/indirect/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu/subdir"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

exceptOnWindows :: Applicative f => f () -> f ()
exceptOnWindows act
  | os == "mingw32" = pure ()
  | otherwise = act
