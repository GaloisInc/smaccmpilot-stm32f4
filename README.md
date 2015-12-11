[smaccmpilot-stm32f4](http://github.com/galoisinc/smaccmpilot-stm32f4)
==============================
[![Build Status](https://travis-ci.org/GaloisInc/smaccmpilot-stm32f4.svg?branch=master)](https://travis-ci.org/GaloisInc/smaccmpilot-stm32f4)

## Overview

The [SMACCMPilot project][smaccmpilot.org] is an embedded systems software
research project by [Galois, Inc][galois]. We're building open-source autopilot
software for small unmanned aerial vehicles (UAVs) using new high-assurance
software methods.

Complete documentation for this repository and related SMACCMPilot work is
available at [smaccmpilot.org][].

Developers are encouraged to [join our mailing list][list] for project
updates. You can also [file issues][issues] on Github.

[galois]: http://galois.com
[smaccmpilot.org]: http://smaccmpilot.org
[list]: http://community.galois.com/mailman/listinfo/smaccmpilot
[issues]: https://github.com/galoisinc/smaccmpilot-stm32f4/issues

## Contents

- The `src` directory contains a collection of Cabal packages for components
  of the SMACCMPilot build. There is a `README` in each package directory
  describing the libraries and executables provided.

## Dependencies

This repository has several external dependencies which may be burdensome to
install. For convenience, we have provided a [smaccmpilot-build][] repository
which packages this repository and all of the required dependencies using git
submodules. We recommend you [clone smaccmpilot-build][smaccmpilot-build], or
provide the following environment variables pointing to the following
repositories:

| name | env variable |
|------|--------------|
| [ivory][] | `IVORY_REPO` |
| [tower][] | `TOWER_REPO` |
| [ivory-tower-stm32][] | `BSP_REPO` |
| [gidl][] | `GIDL_REPO` |
| [gec][] | `GEC_REPO` |

[smaccmpilot-build]: https://github.com/galoisinc/smaccmpilot-build
[ivory]: https://github.com/galoisinc/ivory
[tower]: https://github.com/galoisinc/tower
[ivory-tower-stm32]: https://github.com/galoisinc/ivory-tower-stm32
[gec]: https://github.com/galoisinc/gec
[gidl]: https://github.com/galoisinc/gidl

### Haskell Tools

This project requires the [GHC Haskell Compiler][ghc] version 7.8.x, and [Cabal][] 1.20 or higher.

*Note: we have dropped support for GHC 7.6!*

[ghc]: https://www.haskell.org/ghc
[Cabal]: https://www.haskell.org/cabal

### Cross Compiler

This project expects the [gcc-arm-embedded toolchain][1] to be present in your
`PATH`.

[1]:https://launchpad.net/gcc-arm-embedded

## Compiling

The build system uses recursive makefiles. Individual cabal packages each have
a local makefile that creates a sandbox for building, testing, and generating
Ivory/Tower programs.

Ivory/Tower programs are Haskell programs which generate C sources and an
appropriate makefile. We call these makefiles recursively to build binaries from
the generated C sources.

