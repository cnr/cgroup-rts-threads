module System.CGroup.V2.CGroupSpec (
  tests,
) where

import Control.Monad.IO.Class (liftIO)
import Path
import Path.IO (resolveFile')
import System.CGroup.V2.CGroup
import System.Info (os)
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

-- This test won't work on Windows, because paths starting with `/` are invalid
tests :: Spec
tests = exceptOnWindows $ do
  describe "resolveCGroup" $ do
    it "should resolve a root cgroup" $ do
      cgroup <- resolveFile' "test/System/CGroup/V2/testdata-cgroup/root/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/V2/testdata-cgroup/root/mountinfo"
      expectedRoot <- parseAbsDir "/sys/fs/cgroup"
      expectedLeaf <- parseRelDir "."

      result <- liftIO $ resolveCGroup' cgroup mountinfo
      result `shouldBe` CGroup{cgroupRoot = expectedRoot, cgroupLeaf = expectedLeaf}

    it "should resolve a nested cgroup" $ do
      cgroup <- resolveFile' "test/System/CGroup/V2/testdata-cgroup/nested/cgroup"
      mountinfo <- resolveFile' "test/System/CGroup/V2/testdata-cgroup/nested/mountinfo"
      expectedRoot <- parseAbsDir "/sys/fs/cgroup"
      expectedLeaf <- parseRelDir "some/nested/cgroup"

      result <- liftIO $ resolveCGroup' cgroup mountinfo
      result `shouldBe` CGroup{cgroupRoot = expectedRoot, cgroupLeaf = expectedLeaf}

exceptOnWindows :: Applicative f => f () -> f ()
exceptOnWindows act
  | os == "mingw32" = pure ()
  | otherwise = act
