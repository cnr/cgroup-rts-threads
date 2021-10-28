# cgroup-capabilities

![Hackage](https://img.shields.io/hackage/v/cgroup-capabilities)
![CI](https://github.com/cnr/cgroup-capabilities/actions/workflows/build.yml/badge.svg)

This library sets the number of runtime threads ("capabilities") in GHC's RTS. It is meant to be used as a replacement for [the `-N` RTS flag][rts-n].

Similar to the `-N` RTS flag, this library considers the number of cpu cores (as reported by `GHC.Conc.getNumProcessors`) to set this number.

Unlike the `-N` RTS flag, this library observes the process' [cgroup cpu quota][cgroup-quota] to constrain the number of runtime threads, as applicable.

When running outside of a cgroup, or on a platform other than linux, this library matches the behavior of `-N`.

See the [Why?](#why) section for details.

## Usage

1. Remove `-N` from your executable's `ghc-options`. In `yourproject.cabal`:

```cabal-config
-- before
executable my-executable
  ghc-options: -threaded -with-rtsopts=-N

-- after
executable my-executable
  ghc-options: -threaded
```

2. In your program's `main` function, call `initCapabilities`:

```haskell
module Main (main) where

import Control.Concurrent.CGroup (initCapabilities)

main :: IO ()
main = do
  initCapabilities
  [...]
```

## Why?

It's common in containerized environments to limit cpu consumption of individual containers. There are two primary ways of doing this:

1. The `cpuset` option within a cgroup, which can be used to pin a process to specific cpu cores.
2. The `cfs.cpu_quota_us` option within a cgroup, which can be used to set a limit on the cpu time a process is allowed to consume.

The GHC threaded RTS offers [a flag, `-N`,][rts-n] that can be used to automatically determine the number of "capabilities" (threads) to use, based on the number of physical processors.

The `-N` flag, [as of GHC `9.0.1`][cpuset-commit], respects the `cpuset` option when automatically determining the number of capabilities to use.

Unfortunately, GHC's RTS **does not support cgroup `cfs` quotas**. This leads to substantially degraded performance when there's a large disparity between a `cfs` quota and the number of physical cpu cores -- a very common scenario in, e.g., production kubernetes clusters.

[cpuset-commit]: https://gitlab.haskell.org/ghc/ghc/-/commit/4413828b7c507872c56719fb8920e1c2322830f8
[rts-n]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/using-concurrent.html#rts-options-for-smp-parallelism
[cgroup-quota]: https://www.kernel.org/doc/html/latest/scheduler/sched-bwc.html#management
