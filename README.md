[smaccmpilot-stm32f4](http://github.com/galoisinc/smaccmpilot-stm32f4)
==============================

The [SMACCMPilot project][smaccmpilot.org] is an embedded systems software
research project by [Galois, Inc][galois]. We're building open-source autopilot
software for small unmanned aerial vehicles (UAVs) using new high-assurance
software methods.

Complete documentation for this repository and related SMACCMPilot work is
available at [smaccmpilot.org][].


Developers are encouraged to [join our mailing list][list] for project
updates.

[galois]: http://corp.galois.com
[smaccmpilot.org]: http://smaccmpilot.org
[list]: http://community.galois.com/mailman/listinfo/smaccmpilot

## Contents

This repository contains both the flight controller implementation and board
support package for the SMACCMPilot project. This includes both C sources and
Haskell sources for [Ivory language][ivory] programs.

[ivory]: http://smaccmpilot.org/software/ivory-overview.html

#### Apps

The `/apps` directory contains sources for building applications. The
primary SMACCMPilot flight controller application is called `stabilize`,
other applications are for development or testing.

Note that test application sources exist elsewhere in the tree as well.

#### Src

The `/src` directory contains the C and Ivory sources of the SMACCMPilot
flight library, the MAVLink implementation, and board support package.

## Dependencies

This repository has several external dependencies which may be burdensome to
install. For convenience, we have provided a [smaccmpilot-build][] repository
which packages this repository and all of the required dependencies using git
submodules. We recommend you [clone smaccmpilot-build][smaccmpilot-build] and
follow the documentation instructions for [prerequisites][] and [building][].

[smaccmpilot-build]: http://github.com/galoisinc/smaccmpilot-build
[prerequisites]: http://smaccmpilot.org/software/prerequisites.html
[building]: http://smaccmpilot.org/software/build.html

### Ivory Language

This repository includes Haskell sources for [Ivory language][ivory] programs.
The Ivory language compiler will generates C sources as part of the build
process.

You'll need to install the `smaccpilot` Cabal package in the `/ivory` subdirectory
and provide the [Ivory language][ivory-lang] and [Tower framework][tower]
dependencies.

[ivory-lang]: http://github.com/galoisinc/ivory
[tower]: http://github.com/galoisinc/tower

The Ivory language requires the [GHC Haskell Compiler][ghc] version 7.6.2 or
greater.

[ghc]: http://www.haskell.org/ghc

### Cortex-M4 Toolchain

Download and unpack the [gcc-arm-embedded toolchain][1].

[1]:https://launchpad.net/gcc-arm-embedded

### FreeRTOS

[Download the latest FreeRTOS][2] release. We're using version 7.3.0.

Unpack the ZIP file in a directory near the smaccmpilot-stm32f4 tree: the same
parent directory is best. We will refer to the path of the unzipped source
tree during build configuration.

[2]: http://sourceforge.net/projects/freertos/files/

### ArduPilot

Many of the libraries in the SMACCMPilot software are from the ArduPilot
project.

Clone the [Galois fork of the ArduPilot repo][22], using the `master` branch.
Keep this clone in a directory near the smaccmpilot-stm32f4 tree, as you did
with the FreeRTOS sources.

[22]:https://github.com/GaloisInc/ardupilot.git

## Configuration File

  1. Copy `Config.mk.example` to `Config.mk`.  Open this file in a text editor.
  2. Set `CONFIG_CORTEX_M4_PREFIX` to the location of the Cortex-M4 toolchain.
  3. Copy `Keys.mk.example` to `Keys.mk`

## Compiling

  1 Simply run `make` from the top-level directory.  You can also specify
    a specific target such as `libhwf4.a`.
  2 Build output is placed in the `build` directory. Images and other artifacts
    are in the `build/{platform}_{os}/img` directory, where `{platform}` is one
    of:
    * `px4fmu17_ioar`: PX4FMU 1.7 with the IOAR expansion board,
            for the  Drone based copter
    * `px4fmu17_bare`: PX4FMU 1.7 without an IO expansion board,
              for a radio control ESC based copter like the 3DR ArduCopter Quad
    and `{os}` is one of:
    * `freertos`: Produces complete images using the FreeRTOS operating system
    * `aadl`: Produces applications as libraries, and system description output in
      the Architecture Analysis and Design Language (AADL), for use with other
      operating systems

