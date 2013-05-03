## SMACCM MAVLink generator ##

By Pat Hickey <pat@galois.com>, Galois Inc. 15 Jan 2013

This code is copied or derived from the mavlink project, and therefore is
licensed under the GPL.

### Purpose ###

Provides a tool to translate mavlink message xml specifications, found in
`message_definitions/`, into Ivory modules.

### Context ###

This tool will generate Ivory language modules in the haskell namespace
`Smaccm.Mavlink.Messages.<messagename>`

The Ivory compiler will generate portions of the C `smavlink` library
implementation, which is found in the *smaccmpilot-stm32f4* repository.  The
generated code depends on C headers and source files which are checked in as
part of the `smavlink` library.

