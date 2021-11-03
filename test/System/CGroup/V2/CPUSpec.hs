module System.CGroup.V2.CPUSpec (
  tests,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Ratio ((%))
import Path
import Path.IO (resolveDir')
import System.CGroup.V2.CGroup
import System.CGroup.V2.CPU
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

tests :: Spec
tests = do
  describe "getEffectiveCPUQuota" $ do
    it "should return NoQuota for the root cgroup" $ do
      root <- resolveDir' "test/System/CGroup/V2/testdata-cpu"
      leaf <- parseRelDir "."
      let cgroup =
            CGroup
              { cgroupRoot = root
              , cgroupLeaf = leaf
              }
      quota <- liftIO $ getEffectiveCPUQuota cgroup
      quota `shouldBe` NoQuota

    it "should return NoQuota when there is no quota" $ do
      root <- resolveDir' "test/System/CGroup/V2/testdata-cpu"
      leaf <- parseRelDir "max"
      let cgroup =
            CGroup
              { cgroupRoot = root
              , cgroupLeaf = leaf
              }

      single <- liftIO $ getCPUQuota cgroup
      single `shouldBe` NoQuota

      effective <- liftIO $ getEffectiveCPUQuota cgroup
      effective `shouldBe` NoQuota

    it "should return a simple quota" $ do
      root <- resolveDir' "test/System/CGroup/V2/testdata-cpu"
      leaf <- parseRelDir "limited"
      let cgroup =
            CGroup
              { cgroupRoot = root
              , cgroupLeaf = leaf
              }

      single <- liftIO $ getCPUQuota cgroup
      single `shouldBe` CPUQuota (1 % 5)

      effective <- liftIO $ getEffectiveCPUQuota cgroup
      effective `shouldBe` CPUQuota (1 % 5)

    it "should defer to parent quotas when there is no quota" $ do
      root <- resolveDir' "test/System/CGroup/V2/testdata-cpu"
      leaf <- parseRelDir "limited/unlimited"
      let cgroup =
            CGroup
              { cgroupRoot = root
              , cgroupLeaf = leaf
              }

      single <- liftIO $ getCPUQuota cgroup
      single `shouldBe` NoQuota

      effective <- liftIO $ getEffectiveCPUQuota cgroup
      effective `shouldBe` CPUQuota (1 % 5)

    it "should respect parent cgroup quotas" $ do
      root <- resolveDir' "test/System/CGroup/V2/testdata-cpu"
      leaf <- parseRelDir "limited/bigger"
      let cgroup =
            CGroup
              { cgroupRoot = root
              , cgroupLeaf = leaf
              }
      single <- liftIO $ getCPUQuota cgroup
      single `shouldBe` CPUQuota (4 % 5)

      effective <- liftIO $ getEffectiveCPUQuota cgroup
      effective `shouldBe` CPUQuota (1 % 5)
