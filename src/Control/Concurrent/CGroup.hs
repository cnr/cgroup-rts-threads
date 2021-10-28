-- | This module provides a container-/cgroup-aware substitute for GHC's RTS
-- @-N@ flag. See 'initRTSThreads'.
module Control.Concurrent.CGroup (
  initRTSThreads,
) where

import Control.Exception (Exception (..), SomeAsyncException (SomeAsyncException), SomeException, catch, throwIO)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.CGroup.CPU (CPUQuota (..), getCPUQuota, resolveCPUController)

-- | A container-/cgroup-aware substitute for GHC's RTS @-N@ flag.
--
-- On most platforms, this sets the number of runtime threads to match the
-- number of physical processors (see 'GHC.Conc.getNumProcessors'), which is the
-- default behavior of the GHC @-N@ flag.
--
-- When running within a cgroup on linux (most often within a container), this
-- observes the current process' cgroup cpu quota to constrain the number of
-- runtime threads.
--
-- See 'CPUQuota'
initRTSThreads :: IO ()
initRTSThreads =
  initRTSThreadsFromCGroup
    `safeCatch` (\(_ :: SomeException) -> defaultInitRTSThreads)

-- | Uses the current process' cgroup cpu quota to set the number of runtime
-- threads.
--
-- Throws an Exception when the current process is not running within a cgroup.
initRTSThreadsFromCGroup :: IO ()
initRTSThreadsFromCGroup = do
  cpuController <- resolveCPUController
  cgroupCpuQuota <- getCPUQuota cpuController
  case cgroupCpuQuota of
    NoQuota -> defaultInitRTSThreads
    CPUQuota quota period -> do
      procs <- getNumProcessors
      let capabilities = clamp 1 procs (quota `div` period)
      setNumCapabilities capabilities

-- | Set number of runtime threads to the number of available processors. This
-- matches the behavior of GHC's RTS @-N@ flag.
defaultInitRTSThreads :: IO ()
defaultInitRTSThreads = setNumCapabilities =<< getNumProcessors

-- | Clamp a value within a range
clamp :: Int -> Int -> Int -> Int
clamp lower upper = max lower . min upper

-- | Catch non-async exceptions
safeCatch :: Exception e => IO a -> (e -> IO a) -> IO a
safeCatch act hdl = act `catch` (\e -> if isSyncException e then hdl e else throwIO e)

isSyncException :: Exception e => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True
