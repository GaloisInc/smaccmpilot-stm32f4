The SMACCMPilot Build System
============================

The SMACCMPilot firmware is written in a combination of C, C++, and C
code generated from the Ivory language.  This document describes the
build system that cross compiles these sources to the target
architecture.

## Build Configuration

Configurable aspects of the build are set by writing a `Config.mk`
file at the root of the source tree.  The usual process is to copy
`Config.mk.example` to `Config.mk` and edit this file.

This file contains the settings for which architecture and board to
compile for, the location of the GNU toolchain and libraries such
as FreeRTOS, and controls optional features such as verification with
CBMC.

## Top Level

The build system is built with GNU Make, using a non-recursive style.

The top level Makefile loads the configuration file `Config.mk` and
includes architecture-specific Makefiles from the `mk/` directory.  It
also contains generic rules for compiling C and C++ programs, placing
build artifacts in the `build/<arch>/<board>` directory.

It is safe to delete the `build/` directory to ensure a clean build
(indeed, this is what "make clean" does.)

Finally, the main Makefile searches all subdirectories for files named
`build.mk`, which contain rules used to build targets in that
directory.  By default, all such targets are built (when the user runs
`make` with no target).

The order in which `build.mk` files are included is not defined, so it
is not safe to assume that variables defined in one `build.mk` will be
defined in another unless "recursive variables" are used.

## Sub-Directory "build.mk" Makefiles

Each `build.mk` file contains rules to build one or more executables
and libraries.  Macros from the include files in `mk/` are used to
automate the process of compiling and linking a firmware image or
building a static library.

### Building a Library

(Recommended example file: `src/bsp/hwf4/src/build.mk`)

To build a library in `build.mk`, first choose a unique variable
prefix for the library.  All variables set in this `build.mk` will
start with this prefix (since all `build.mk` files are included in a
single invocation of `make`, we must be careful of the single, global
namespace for variables).

We set several variables describing the name, compiler flags, and
object files for the library.  For example, to build a library called
`libhw.a` from several object files, using a variable prefix `HW`:

    HW_LIB     := libhw.a
    HW_CFLAGS  := -DBUILDING_HW -Wall -I$(TOP)/include/drivers
    HW_OBJECTS := spi.o i2c.o uart.o

Then, to generate `make` rules for this library, call the `library`
macro using `eval`:

    $(eval $(call library,HW))

This macro takes the prefix given in its argument (in this case, `HW`)
and looks for the following variables derived from it:

- `<prefix>_LIB`           the name of the static library to build
- `<prefix>_CFLAGS`        options to pass to the C compiler
- `<prefix>_CXXFLAGS`      options to pass to the C++ compiler
- `<prefix>_OBJECTS`       object files to compile (relative to the directory
                           `build.mk` is in)
- `<prefix>_REAL_OBJECTS`  object files to compile (relative to the top
                           of the source tree)

Rules will be automatically generated to compile sources into object
files and combine them into a library.

### Building a Firmware Image

(Recommended example file: `src/bsp/hwf4/tests/spi/build.mk`)

Building a firmware image is much like building a library, except the
name of the macro used and set of variables supported is different.

For example, building an image that uses the `libhw.a` library defined
in the previous section and links with an external library:

    SPI_TEST_IMG       := spitest
    SPI_TEST_OBJECTS   := main.o

    SPI_TEST_CFLAGS    += -I$(TOP)/include/hw
    SPI_TEST_CFLAGS    += $(FREERTOS_CFLAGS)
    SPI_TEST_LIBRARIES := libhw.a
    SPI_TEST_LIBS      := -lFreeRTOS

    $(eval $(call image,SPI_TEST))

The set of variables supported by `image` is:

- `<prefix>_IMG`           name of the firmware image to build
- `<prefix>_OBJECTS`       objects, relative to `build.mk`
- `<prefix>_REAL_OBJECTS`  objects, relative to top of src dir
- `<prefix>_CFLAGS`        C compiler flags
- `<prefix>_CXXFLAGS`      C++ compiler flags
- `<prefix>_LDFLAGS`       linker flags
- `<prefix>_LIBRARIES`     built libs to use (eg: libfoo.a)
- `<prefix>_LIBS`          system libs to use (eg: -lfoo)

Note the two different variables for libraries.  Use `_LIBRARIES` when
the library is built as part of the SMACCMPilot tree, and the firmware
image will correctly be rebuilt when the library changes.  Use `_LIBS`
when the library is a "system library" and unlikely to change (and
should not be tracked as a dependency).

The `image` macro generates rules to build an ELF executable from the
object files, a binary image of the ELF executable suitable for
loading using a JTAG interface, and (if appropriate for the target
architecture) a firmware image that can be loaded using a boot loader.

### Generating Ivory Source

(Recommended example file: `apps/bsp-led-test/build.mk`)

For subprojects that contain Ivory source, there is another macro used
to automate the code generation process.  All we need to know is the
name of the generator program built in the "ivory" project.  It is
assumed that the generator is already built (usually by running `make`
in the Ivory repository).

The `ivory_pkg` macro creates rules to generate sources by running the
generator program, and sets variables that can be used in an `image`
or `library` subproject.

Let's say our `spitest` program is modified to contain some Ivory
source in addition to "main.c".  The Ivory generator executable is
called `spitest-gen`.  Here's what the new `build.mk` file looks like:

    $(eval $(call ivory_pkg,IVORY_PKG_SPI_TEST,spitest-gen))

    SPI_TEST_IMG          := spitest
    SPI_TEST_OBJECTS      := main.o
    SPI_TEST_REAL_OBJECTS += $(IVORY_PKG_SPI_TEST_OBJECTS)

    SPI_TEST_CFLAGS       += -I$(TOP)/include/hw
    SPI_TEST_CFLAGS       += $(FREERTOS_CFLAGS)
    SPI_TEST_CFLAGS       += $(IVORY_PKG_SPI_TEST_CFLAGS)
    SPI_TEST_LIBRARIES    := libhw.a
    SPI_TEST_LIBS         := -lFreeRTOS

    $(eval $(call image,SPI_TEST))

The call to `ivory_pkg` at the top sets up the rules for code
generation, and sets variables prefixed by its first argument
(`IVORY_PKG_SPI_TEST` in this case).  These variables are used to
build an image as normal using `image`.

The second argument to `ivory_pkg` is the name of the generator
program, which must be in `$(CONFIG_CABAL_SANDBOX)/dist/bin`.

The variables set by `ivory_pkg` are:

- `<prefix>_CFLAGS`      C compiler flags to use
- `<prefix>_OBJECTS`     object files of generated sources

Note that the Ivory source files are not relative to the current
directory (they are stored in `build/<arch>/<board>/gen`), so
they must be added to `<prefix>_REAL_OBJECTS` when building an image
or library.  It would be nice if this was automatic, but the current
system does not support this.
