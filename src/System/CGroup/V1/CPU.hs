{-# LANGUAGE TemplateHaskell #-}

-- | Types and operations for the CPU cgroup controller.
module System.CGroup.V1.CPU (
  -- * Operations on the CPU controller
  getProcessCPUQuota,
  getCPUQuota,
  CPUQuota (..),

  -- * The CPU cgroup controller
  CPU,
  resolveCPUController,
) where

import Control.Monad ((<=<))
import Data.Ratio ((%))
import Path
import System.CGroup.Types (CPUQuota (..))
import System.CGroup.V1.Controller (Controller (..), resolveCGroupController)

-- | The "cpu" cgroup controller
data CPU

-- | Resolve the CPU cgroup controller for the current process
--
-- Throws an Exception if the CPU controller is not able to be found, or when
-- running outside of a cgroup
resolveCPUController :: IO (Controller CPU)
resolveCPUController = resolveCGroupController "cpu"

-- | Get the current process' CPU quota
getProcessCPUQuota :: IO CPUQuota
getProcessCPUQuota = getCPUQuota =<< resolveCPUController

-- | Read "cpu.cfs_quota_us" and "cpu.cfs_period_us" files into a CPUQuota.
--
-- For example:
--
-- @
-- | cpu.cfs_quota_us | cpu.cfs_period_us | CPUQuota         |
-- | ---------------- | ----------------- | ---------------- |
-- |           100000 |            100000 | CPUQuota (1 % 1) |
-- |           200000 |            100000 | CPUQuota (2 % 1) |
-- |            50000 |            100000 | CPUQuota (1 % 2) |
-- |               -1 |            100000 | NoQuota          |
-- @
getCPUQuota :: Controller CPU -> IO CPUQuota
getCPUQuota (Controller root) = do
  quota <- readCGroupInt (root </> cpuQuotaPath)
  period <- readCGroupInt (root </> cpuPeriodPath)
  case quota of
    (-1) -> pure NoQuota
    _ -> pure (CPUQuota (quota % period))

-- | Read a cgroup configuration value from its file
readCGroupInt :: Path b File -> IO Int
readCGroupInt = readIO <=< (readFile . toFilePath)

-- | Path to the "cpu quota" file
--
-- When this file contains "-1", there is no quota set
cpuQuotaPath :: Path Rel File
cpuQuotaPath = $(mkRelFile "cpu.cfs_quota_us")

-- | Path to the "cpu period" file
cpuPeriodPath :: Path Rel File
cpuPeriodPath = $(mkRelFile "cpu.cfs_period_us")
