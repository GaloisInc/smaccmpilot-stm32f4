## SMACCM MAVLink generator ##

By Pat Hickey <pat@galois.com>, Galois Inc. 15 Jan 2013

This code is copied or derived from the mavlink project, and therefore is
licensed under the GPL.

### Purpose ###

Provides a tool to translate mavlink message xml specifications into Ivory
modules.

This tool will generate Ivory language modules in the haskell namespace
`SMACCMPilot.Mavlink`, with each message defined in a separate haskell file
`SMACCMPilot.Mavlink.Messages.<messagename>`.

