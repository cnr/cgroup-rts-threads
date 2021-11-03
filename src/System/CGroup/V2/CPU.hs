{-# LANGUAGE TemplateHaskell #-}

-- | Types and operations for CPU-related data within a cgroup.
module System.CGroup.V2.CPU (
  getProcessEffectiveCPUQuota,
  getEffectiveCPUQuota,
  getCPUQuota,
  CPUQuota (..),
) where

import Data.Ratio
import Path
import System.CGroup.Types (CPUQuota (..))
import System.CGroup.V2.CGroup
import System.Directory (doesFileExist)

-- | Get the current process' effective CPU quota
--
-- See 'getEffectiveCPUQuota'
getProcessEffectiveCPUQuota :: IO CPUQuota
getProcessEffectiveCPUQuota = getEffectiveCPUQuota =<< resolveCGroup

-- | Compute the "effective CPU quota" fr a cgroup, which may be smaller than
-- the given cgroup's individual quota.
--
-- When a parent (or grandparent, etc) of this cgroup has a lower cpu quota,
-- the lower quota is returned instead.
getEffectiveCPUQuota :: CGroup -> IO CPUQuota
getEffectiveCPUQuota cgroup = do
  quotas <- cpuQuotasUntilRoot cgroup
  pure (foldr min NoQuota quotas)

-- | Read this specific cgroup's "cpu.max" file into a CPUQuota.
--
-- For example:
--
-- @
-- | cpu.max        | quota            |
-- | -------------- | ---------------- |
-- |  100000 100000 | CPUQuota (1 % 1) |
-- |  200000 100000 | CPUQuota (2 % 1) |
-- |   50000 100000 | CPUQuota (1 % 2) |
-- |     max 100000 | NoQuota          |
-- @
--
--
-- Returns NoQuota for the root cgroup, or when the cpu controller is not enabled in this cgroup.
--
-- __Most often, you'll want to use 'getEffectiveCPUQuota' instead.__
getCPUQuota :: CGroup -> IO CPUQuota
getCPUQuota cgroup = do
  let path = cgroupRoot cgroup </> cgroupLeaf cgroup </> cpuMaxPath
  exists <- doesFileExist (toFilePath path)
  if not exists
    then pure NoQuota
    else do
      content <- readFile (toFilePath path)
      case words content of
        ["max", _] -> pure NoQuota
        [numText, denText] -> do
          num <- readIO numText
          den <- readIO denText
          pure (CPUQuota (num % den))
        _ -> fail "Couldn't parse cpu.max"

-- | Get the parent cgroup. Returns Nothing when the provided cgroup is the root
-- cgroup.
getParentCGroup :: CGroup -> Maybe CGroup
getParentCGroup cgroup =
  if cgroupLeaf cgroup == parent (cgroupLeaf cgroup)
    then Nothing
    else Just cgroup{cgroupLeaf = parent (cgroupLeaf cgroup)}

-- | Return all CPU quotas from all parent cgroups
cpuQuotasUntilRoot :: CGroup -> IO [CPUQuota]
cpuQuotasUntilRoot = traverse getCPUQuota . cgroupsUntilRoot

-- | Return the list of cgroups up to but excluding the root cgroup
cgroupsUntilRoot :: CGroup -> [CGroup]
cgroupsUntilRoot = iterateMaybe getParentCGroup

-- | like 'iterate', but terminated when @Nothing@ is returned by the provided
-- function
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

cpuMaxPath :: Path Rel File
cpuMaxPath = $(mkRelFile "cpu.max")
