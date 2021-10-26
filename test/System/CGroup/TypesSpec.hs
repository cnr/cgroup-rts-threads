module System.CGroup.TypesSpec (
  tests,
) where

import Control.Monad.IO.Class (liftIO)
import Path
import Path.IO (resolveFile')
import System.CGroup.Types
import System.Info (os)
import Test.Sandwich (CoreSpec, describe, it, shouldBe)

-- This test won't work on Windows, because paths starting with `/` are invalid
tests :: CoreSpec
tests = exceptOnWindows $ do
  describe "resolveGroupController" $ do
    it "should work on a real world example" $ do
      cgroup <- resolveFile' "test/System/CGroup/testdata-types/realworld/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/testdata-types/realworld/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should resolve a direct mount root" $ do
      cgroup <- resolveFile' "test/System/CGroup/testdata-types/direct/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/testdata-types/direct/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should resolve subdirectories of a mount root" $ do
      cgroup <- resolveFile' "test/System/CGroup/testdata-types/indirect/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/testdata-types/indirect/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu/subdir"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

    it "should work for cgroups v2" $ do
      cgroup <- resolveFile' "test/System/CGroup/testdata-types/cgroupsv2/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/testdata-types/cgroupsv2/mountinfo"
      expected <- parseAbsDir "/sys/fs/cgroup/cpu"

      controller <- liftIO $ resolveCGroupController' cgroup mountinfo "cpu"
      controller `shouldBe` Controller expected

exceptOnWindows :: Applicative f => f () -> f ()
exceptOnWindows act
  | os == "mingw32" = pure ()
  | otherwise = act
