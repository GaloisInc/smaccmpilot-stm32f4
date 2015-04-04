
# smaccm-datalink

Library for SMACCMPilot project datalink. Defines a framing protocol used over
a raw serial link (or, optionally, over HM-TRP radios using on the smaccm-sik
firmware). Library contains native Haskell and Ivory language implementations
of this protocol.


## Building

This library and its tests should be built using the included `Makefile`.
Run the `make create-sandbox` target first to create a cabal sandbox in this
directory and install all dependencies.

Run the `make test` target to build all tests. Individual tests are described
below.

## Tests

### `smaccm-datalink-ivory-frameloopback-test`

An Ivory/Tower application for STM32 microcontrollers which implements a
loopback interface for the smaccm-datalink framing protocol. Use this with
the corresponding `native-frameloopback-client`.

### `smaccm-datalink-ivory-commsecloopback-test`

An Ivory/Tower application for STM32 microcontrollers which implements a
loopback interface for the smaccm-datalink framing protocol and the
smaccm-commsec protocol. Uses the configuration options in the `symmetric_key`
section of the build config file (by default, `default.conf`) for the
smaccm-commsec protocol stack, following the convention that the embedded
controller acts as the server.

Use this with the corresponding `native-frameloopback-client`.

### `smaccm-datalink-native-serial-test`

A native Haskell application for debugging smaccm-datalink framed protocols,
and sending special commands to HM-TRP radios based on the smaccm-sik firmware.

### `smaccm-datalink-native-frameloopback-client`

A native Haskell application. Sends 20 smaccm-datalink framed packets with
randomly generated contents. Expects to receive the same packets back, in the
same order.

### `smaccm-datalink-native-commsecloopback-client`

A native Haskell application. Sends 20 smaccm-datalink framed, smaccm-commsec
encrypted packets, from randomly generated plaintext contents. Expects to
receive the same plaintexts back, in the same order. Uses the  `symmetric_key`
section of the build config file (e.g. `default.conf`) to configure the
smaccm-commsec stack, using the convention that PC applications are the client.

### `smaccm-datalink-repl`

A native Haskell application. Presents an interactive prompt that allows the
user to send smaccm-datalink frames of arbitrary length, and inspect
communications with a serial port.

