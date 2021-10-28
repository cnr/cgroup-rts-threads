{-# LANGUAGE TemplateHaskell #-}

-- | Types and operations for the CPU CGroup controller.
module System.CGroup.CPU (
  -- * The CPU cgroup controller
  CPU,
  resolveCPUController,

  -- * Operations on the CPU controller
  CPUQuota (..),
  getCPUQuota,
) where

import Control.Monad ((<=<))
import Path
import System.CGroup.Controller (Controller (..), resolveCGroupController)

-- | The "cpu" cgroup controller
data CPU

-- | Resolve the CPU cgroup controller for the current process
--
-- Throws an Exception if the CPU controller is not able to be found, or when
-- running outside of a cgroup
resolveCPUController :: IO (Controller CPU)
resolveCPUController = resolveCGroupController "cpu"

-- | A CPU quota is the amount of CPU time our process can use relative to the
-- scheduler period
--
-- For example:
--
-- @
-- | cpu.cfs_quota_us | cpu.cfs_period_us | description |
-- | ---------------- | ----------------- | ----------- |
-- |           100000 |            100000 | (1)         |
-- |           200000 |            100000 | (2)         |
-- |            50000 |            100000 | (3)         |
-- |               -1 |            100000 | (4)         |
-- @
--
-- (1): we can use up to a single CPU core
--
-- (2): we can use up to two CPU cores
--
-- (3): the scheduler will give us a single CPU core for up to 50% of the time
--
-- (4): we can use all available CPU resources (there is no quota)
data CPUQuota
  = NoQuota
  | -- | cpu.cfs_quota_us, cpu.cfs_period_us
    CPUQuota Int Int
  deriving (Eq, Ord, Show)

-- | Read a CGroup configuration value from its file
readCGroupInt :: Path b File -> IO Int
readCGroupInt = readIO <=< (readFile . toFilePath)

-- | Get the CPU quota within the given cgroup CPU controller
getCPUQuota :: Controller CPU -> IO CPUQuota
getCPUQuota (Controller root) = do
  quota <- readCGroupInt (root </> cpuQuotaPath)
  case quota of
    (-1) -> pure NoQuota
    _ -> CPUQuota quota <$> readCGroupInt (root </> cpuPeriodPath)

-- Path to the "cpu quota" file
--
-- When this file contains "-1", there is no quota set
cpuQuotaPath :: Path Rel File
cpuQuotaPath = $(mkRelFile "cpu.cfs_quota_us")

-- Path to the "cpu period" file
cpuPeriodPath :: Path Rel File
cpuPeriodPath = $(mkRelFile "cpu.cfs_period_us")
