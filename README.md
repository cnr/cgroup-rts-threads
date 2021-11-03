# cgroup-rts-threads

![Hackage](https://img.shields.io/hackage/v/cgroup-rts-threads)
![tests](https://github.com/cnr/cgroup-rts-threads/actions/workflows/tests.yml/badge.svg)

This library provides a container-/cgroup-aware substitute for GHC's [RTS `-N` flag][rts-n], used to set the number of runtime threads.

Similar to the RTS `-N` flag, this library considers the number of cpu cores (as reported by `GHC.Conc.getNumProcessors`) to set this number.

Unlike the RTS `-N` flag, this library observes the process' cgroup cpu quota to constrain the number of runtime threads, as applicable.

When running outside of a cgroup, or on a platform other than linux, this library matches the behavior of `-N`.

See the [Why?](#why) section for details.

## Usage

1. Remove `-N` from your executable's `rtsopts`. In `yourproject.cabal`:

```cabal-config
-- before
executable my-executable
  ghc-options: -threaded -with-rtsopts=-N

-- after
executable my-executable
  ghc-options: -threaded
```

2. In your program's `main` function, call `initRTSThreads`:

```haskell
module Main (main) where

import Control.Concurrent.CGroup (initRTSThreads)

main :: IO ()
main = do
  initRTSThreads
  [...]
```

## Why?

It's common in containerized environments to limit cpu consumption of individual containers. There are two primary ways of doing this:

1. Configure a cgroup's `cpuset` to pin a process to specific cpu cores.
2. Configure a cgroup's cpu quota ([`cfs` under cgroups v1][cgroup-quota-v1] or [`cpu.max` under cgroups v2][cgroup-quota-v2]) to set a limit on the cpu time a process is allowed to consume.

The GHC threaded RTS offers [a flag, `-N`,][rts-n] that automatically determines the number of threads to use, based on the number of physical processors.

The RTS `-N` flag, [as of GHC `9.0.1`][cpuset-commit], respects the `cpuset` options when determining the number of threads to use.

Unfortunately, the RTS `-N` flag **does not respect cgroup cpu quotas**. This leads to substantially degraded performance when there's a large disparity between a cgroup's cpu quota and the number of physical cpu cores -- a very common scenario in, e.g., production kubernetes clusters.

[cpuset-commit]: https://gitlab.haskell.org/ghc/ghc/-/commit/4413828b7c507872c56719fb8920e1c2322830f8
[rts-n]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/using-concurrent.html#rts-options-for-smp-parallelism
[cgroup-quota-v1]: https://www.kernel.org/doc/html/latest/scheduler/sched-bwc.html#management
[cgroup-quota-v2]: https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v2.html#cpu-interface-files
