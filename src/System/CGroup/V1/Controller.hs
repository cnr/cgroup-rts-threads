-- | Common types and operations for cgroup controllers.
module System.CGroup.V1.Controller (
  -- * cgroup controllers
  Controller (..),
  resolveCGroupController,
  resolveCGroupController',
) where

import Control.Monad (guard)
import Data.Foldable (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Path
import System.CGroup.Types (Mount (..), RawCGroup (..), parseCGroups, parseFile, parseMountInfo)

-- | A cgroup (v1) controller path for a specific subsystem
newtype Controller a = Controller {unController :: Path Abs Dir}
  deriving (Eq, Ord, Show)

-- | Resolve a cgroup (v1) controller by name, as viewed by the current process
--
-- see cgroups(7): \/proc\/self\/cgroup is a file that contains information about
-- control groups applied to this process
--
-- see proc(5): \/proc\/self\/mountinfo is a file that contains information about
-- mounts available to this process
--
-- Throws an Exception when the controller is not able to be found, or when
-- running outside of a cgroup
resolveCGroupController :: Text -> IO (Controller a)
resolveCGroupController controller = do
  cgroupPath <- parseAbsFile "/proc/self/cgroup"
  mountinfoPath <- parseAbsFile "/proc/self/mountinfo"
  resolveCGroupController' cgroupPath mountinfoPath controller

-- | Resolve a cgroup controller by name, under the given cgroup and
-- mountinfo paths
--
-- Throws an Exception when the controller is not able to be found, or when
-- running outside of a cgroup
resolveCGroupController' :: Path Abs File -> Path Abs File -> Text -> IO (Controller a)
resolveCGroupController' cgroupPath mountinfoPath controllerName = do
  cgroups <- parseFile parseCGroups cgroupPath
  mounts <- parseFile parseMountInfo mountinfoPath
  cgroup <- maybe (fail "Couldn't find cgroup for controller") pure (findMatchingCGroup controllerName cgroups)
  resolved <- maybe (fail "Couldn't find mount for cgroup") pure (resolveControllerMountPath controllerName cgroup mounts)
  pure (Controller resolved)

-- | Find a cgroup for a specific controller (cgroups v1)
findMatchingCGroup :: Text -> [RawCGroup] -> Maybe RawCGroup
findMatchingCGroup controllerName = find containsController
  where
    containsController :: RawCGroup -> Bool
    containsController = (controllerName `elem`) . rawCGroupControllers

-- | Find a Mount matching a controller name and cgroup, returning the absolute
-- resolved path of a controller
resolveControllerMountPath :: Text -> RawCGroup -> [Mount] -> Maybe (Path Abs Dir)
resolveControllerMountPath controllerName cgroup = firstMaybe (tryResolveMount controllerName cgroup)

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f = listToMaybe . mapMaybe f

-- | Attempt to match a cgroup controller to a mount, returning the absolute
-- resolved path of the controller
--
-- Returns Nothing if the mount does not match the cgroup controller
--
-- A matching mount must have a filesystem type of "cgroup" and contain the
-- controller name within its "super options".
--
-- Per cgroups(7), the cgroup path is relative to a mount root in the process's
-- mount hierarchy. Notably, a mount root /is not the same as its mount point/.
-- A mount point is the path at which the mount is visible to the process.
--
-- As such, we need to look for a mount whose mount root either..
--
-- - ..exactly matches our cgroup's path, in which case we directly return the
--   mount's mount path; OR
--
-- - ..is a prefix of our cgroup's path, in which case we return the relative
--   path from the mount root appended to the mount's mount path
tryResolveMount :: Text -> RawCGroup -> Mount -> Maybe (Path Abs Dir)
tryResolveMount controllerName cgroup mount = do
  guard ("cgroup" == mountFilesystemType mount)
  guard (controllerName `elem` mountSuperOptions mount)
  mountRootAsPath <- parseAbsDir (Text.unpack (mountRoot mount))
  mountPointAsPath <- parseAbsDir (Text.unpack (mountPoint mount))
  if rawCGroupPath cgroup == mountRootAsPath
    then Just mountPointAsPath
    else do
      rel <- stripProperPrefix mountRootAsPath (rawCGroupPath cgroup)
      Just (mountPointAsPath </> rel)
