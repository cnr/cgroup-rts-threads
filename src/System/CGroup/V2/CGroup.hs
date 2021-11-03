{-# LANGUAGE TemplateHaskell #-}

-- | Common types and operations for cgroups (v2)
module System.CGroup.V2.CGroup (
  CGroup (..),
  resolveCGroup,
  resolveCGroup',
) where

import Data.Foldable (find)
import qualified Data.Text as Text
import Path
import System.CGroup.Types (Mount (..), RawCGroup (..), parseCGroups, parseFile, parseMountInfo)

-- | A cgroup (under cgroups v2)
data CGroup = CGroup
  { -- | The root of the cgroup hierarchy
    cgroupRoot :: Path Abs Dir
  , -- | A specific cgroup's relative path from the cgroup hierarchy root
    cgroupLeaf :: Path Rel Dir
  }
  deriving (Eq, Ord, Show)

-- | Resolve the cgroup (v2) used for the current process
--
-- see cgroups(7): \/proc\/self\/cgroup is a file that contains information
-- about control groups applied to this process
--
-- see proc(5): \/proc\/self\/mountinfo is a file that contains information
-- about mounts available to this process
--
-- Throws an Exception when the cgroup is unable to be found, or when the
-- current process is not running under cgroups v2
resolveCGroup :: IO CGroup
resolveCGroup = do
  cgroupPath <- parseAbsFile "/proc/self/cgroup"
  mountinfoPath <- parseAbsFile "/proc/self/mountinfo"
  resolveCGroup' cgroupPath mountinfoPath

-- | Resolve a cgroup (v2) from the given cgroup and mountinfo files
--
-- Throws an Exception when the cgroup is unable to be found, or when the
-- provided paths do not construct a valid cgroup
resolveCGroup' :: Path Abs File -> Path Abs File -> IO CGroup
resolveCGroup' cgroupPath mountinfoPath = do
  cgroups <- parseFile parseCGroups cgroupPath
  case cgroups of
    -- expect to find a cgroup with hierarchy ID 0 and an empty list of controllers
    [RawCGroup "0" [] cgroupLeafAbs] -> do
      mounts <- parseFile parseMountInfo mountinfoPath
      cgroupRootMount <- maybe (fail "Couldn't find cgroup hierarchy root mount") pure (findCGroupHierarchyRootMount mounts)
      mountPointAsPath <- parseAbsDir (Text.unpack (mountPoint cgroupRootMount))

      case fromAbsDir cgroupLeafAbs of
        "/" ->
          pure
            ( CGroup
                { cgroupRoot = mountPointAsPath
                , cgroupLeaf = $(mkRelDir ".")
                }
            )
        _ -> do
          -- Drop the leading '/' from the cgroup path
          cgroupLeafRel <- parseRelDir (drop 1 (fromAbsDir cgroupLeafAbs))
          pure
            ( CGroup
                { cgroupRoot = mountPointAsPath
                , cgroupLeaf = cgroupLeafRel
                }
            )
    _ -> fail ("Found incompatible cgroups: " <> show cgroups)

-- | Find the cgroups v2 hierarchy root.
--
-- We expect to find a mount with the filesystem type "cgroup2"
findCGroupHierarchyRootMount :: [Mount] -> Maybe Mount
findCGroupHierarchyRootMount = find ((== "cgroup2") . mountFilesystemType)
