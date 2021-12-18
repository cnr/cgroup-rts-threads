-- | This module provides a container-/cgroup-aware substitute for GHC's RTS
-- @-N@ flag. See 'initRTSThreads'.
module Control.Concurrent.CGroup (
  initRTSThreads,
  initRTSThreadsWith,
  RoundQuota (..),
) where

import Control.Exception (Exception (..), SomeAsyncException (SomeAsyncException), SomeException, catch, throwIO)
import qualified Data.Ratio as Ratio
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.CGroup.Types (CPUQuota (..))
import qualified System.CGroup.V1.CPU as V1
import qualified System.CGroup.V2.CPU as V2

-- | CPU quotas can be fractions, but the number of RTS capabilities is an integer. This type
-- determines how to round the CPU quota to get to the number of capabilities.
--
-- The names correspond to the similarly named methods of 'RealFrac'.
--
data RoundQuota
  = CeilingQuota
  | FloorQuota
  | RoundQuota
  deriving (Show, Read, Eq, Ord)

-- | Round a quota.
roundQuota :: RoundQuota -> Ratio.Ratio Int -> Int
roundQuota roundMode =
  case roundMode of
    CeilingQuota -> ceiling
    FloorQuota -> floor
    RoundQuota -> round

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
-- By default, the number of capabilities is determined by rounding the CPU quota down.
--
-- See 'CPUQuota'
initRTSThreads :: IO ()
initRTSThreads = initRTSThreadsWith FloorQuota

-- | Same as 'initRTSThreads' but lets you specify the quota round mode.
initRTSThreadsWith :: RoundQuota -> IO ()
initRTSThreadsWith roundMode = do
  quota <-
    V1.getProcessCPUQuota
      `fallback` V2.getProcessEffectiveCPUQuota
      `fallback` pure NoQuota
  initRTSThreadsFromQuota roundMode quota

-- | Use a CPU quota to set the number of runtime threads.
initRTSThreadsFromQuota :: RoundQuota -> CPUQuota -> IO ()
initRTSThreadsFromQuota _ NoQuota = defaultInitRTSThreads
initRTSThreadsFromQuota roundMode (CPUQuota ratio) = do
  procs <- getNumProcessors
  setNumCapabilities $ clamp 1 procs $ roundQuota roundMode ratio

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

-- | Return the result of the first successful action
fallback :: IO a -> IO a -> IO a
fallback a b = a `safeCatch` (\(_ :: SomeException) -> b)
