# smaccm-comm-schema

This directory is not a cabal package. Instead, it generates cabal packages
impelmenting the SMACCMPilot communications link schema, using the `gidl` tool.

The SMACCMPilot communications link schema is described in the
`smaccm-comm-schema.idl` file found in this directory.

## Building

First, use the Makefile's `create-sandbox` target to install the `gidl` tool in
a local sandbox.

The default target uses `gidl` to generate a complete cabal package for the
native haskell backend.

## Testing

`gidl` generates cabal libraries complete with test suite. The Makefile's `test`
target will install and run the test suite for each generated cabal package.

