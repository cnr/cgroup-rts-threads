module Control.Concurrent.CGroup (
  initCapabilities,
) where

import Control.Exception (Exception (..), SomeAsyncException (SomeAsyncException), SomeException, catch, throwIO)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.CGroup.CPU (CPUQuota (..), getCPUQuota, resolveCPUController)

-- | Sets the number of available capabilities via 'GHC.Conc.setNumCapabilities'
--
-- On most platforms, this sets the number of capabilities to the number of
-- physical processors (see 'GHC.Conc.getNumProcessors').
--
-- When running within a cgroup on linux (most often within a container), this
-- takes into account the available cpu quota
initCapabilities :: IO ()
initCapabilities =
  initCapabilitiesFromCGroup
    `safeCatch` (\(_ :: SomeException) -> defaultInitCapabilities)

-- | Uses the current process' cgroup cfs quota to set the number of available
-- capabilities.
--
-- Throws an Exception when the current process is not running within a cgroup
initCapabilitiesFromCGroup :: IO ()
initCapabilitiesFromCGroup = do
  cpuController <- resolveCPUController
  cgroupCpuQuota <- getCPUQuota cpuController
  case cgroupCpuQuota of
    NoQuota -> defaultInitCapabilities
    CPUQuota quota period -> do
      procs <- getNumProcessors
      let capabilities = clamp 1 procs (quota `div` period)
      setNumCapabilities capabilities

-- | Set number of capabilities to the number of available processors
defaultInitCapabilities :: IO ()
defaultInitCapabilities = setNumCapabilities =<< getNumProcessors

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
