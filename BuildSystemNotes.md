Build System
============

This is intended as a brief overview of the build system.  Users of the system
can mostly ignore the following and just follow build instructions found at
[smaccmpilot.org](http://smaccmpilot.org).

Verbose building: add `V=1` as an argument to see verbose output.

Functions to build most targets are defined in files found in the`mk` directory.
Projects define a `build.mk` that usually calls a macro to define
project-specific targets.

The top-level Makefile mostly only is used to define the platforms and a few
targets.  The platforms are the hardware/software combinations (e.g., FreeRTOS
running on PX4, AADL on PX4, etc.).  (Note that no `.mk` files are imported
except `Config.mk`.  Instead, `mk/main.mk` defines the platform and targets and
is called using recursive make.)  The `TARGET` variable is often empty, in which
case `all` is the default target, so `ALL_TARGETS` defines the collection of
targets.  This includes building libraries, the binaries, and other targets
(e.g., the GCS shell script).

In general, new non-build targets should be added to the `OTHER_TARGETS`
variable in a project's `build.mk` file.

To see how each project's targets are added to these variables, consider
`mk/image.mk`, included in `mk/main.mk`.  In there is a macro `image` that is
called by individual projects in the `build.mk` files.  Note that in the macro,
the variables `IMAGES`, etc. are added to to build the set of targets.

Defining new macros can be tricky with all the variable escaping; try following
an existing project and note a few "tricks" that may help:

- use `V=1`.

- to see the expanded macro, change the `call` to `info`: instead of
    $(eval $(call xxx_pkg(...)))
use
    $(info $(call xxx_pkg(...)))

or just
    $(info $(VAR))
to print a variable's value.

