
# smaccm-mavlink

`smaccm-mavlink` is a Cabal package that provides an implementation of the
[MAVLink protocol][mavlink] for the Ivory language.

[mavlink]: http://qgroundcontrol.org/mavlink/start

The MAVLink protocol specifies a set of messages that are understood by a
variety of flight controllers and Ground Control Stations (GCSs). Each message
is a fixed set of fields, where each field is a fixed size and type.
MAVLink specifies a binary representation for these messages, as well as a
"common" set of messages that individual implementations may extend.

We have extended the common messages to include several SMACCMPilot specific
messages. The MAVLink message specification, in XML format, is provided
separately in our [fork of the mavlink repository][mavlink-fork].

[mavlink-fork]: https://github.com/galoisinc/mavlink

## Contents

This package contains a code generator program that reads a MAVLink message
specification and outputs Haskell files, the most recent set of generated
files, and finally, some hand written Haskell files.

### Code Generator

The code generator is found in the `ivory-module-generator` subdirectory. It is
written in Python and derived from code provided by the MAVLink project -
therefore, all code in that subdirectory is licensed under the GPL.

The user should not ever need to use the code generator directly. Invocation of
the code generator should be done through the `generate` target in the included
`Makefile`. Note that this target depends on our [fork of the mavlink
repository][mavlink-fork] being present at a location described by the
`MAVLINK_REPO` environment variable. By default, it can be located in the same
parent directory as this repository.

The `generate` target in the `Makefile` will delete existing message
definitions, create a new set of message definitions, and modify the
`smaccm-mavlink.cabal` file with a list of the generated haskell sources.

### Generated Code

Separate Haskell files are created in the `src/SMACCMPilot/Messages/` directory
for each MAVLink message.  Each of these haskell source files exports an Ivory
struct definition, an Ivory module definition, Ivory procedures for packing and
unpacking the struct to the binary represntation, and values of the MAVLink
message ID and "CRC Extra" value.

Additionally, we automatically generate the module
`src/SMACCMPilot/Messages.hs`, which exports each message pack and unpack
function, as well as tables mapping each MAVLink message ID to the corresponding
length and "CRC extra" value for that message.

### Additional Code

Other haskell source files in the `src/SMACCMPilot/` subdirectory are written by
hand.

### Tests

At this time, there are no unit tests demonstrating the correct functionality of
the `smaccm-mavlink` library.

An executable `smaccm-mavlink-debugger` is provided with this library. It is a
native Haskell application which will read MAVLink packets in binary
representation off a serial port, and validate MAVLink protocol header and
checksums, and display the raw binary payload of messages.



