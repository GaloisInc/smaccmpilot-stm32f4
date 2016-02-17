# smaccm-comm-schema

This directory is not a cabal package. Instead, it generates cabal packages
impelmenting the SMACCMPilot communications link schema, using the `gidl` tool.

The SMACCMPilot communications link schema is described in the
`smaccm-comm-schema.idl` file found in this directory.

## Building

First, use [`stack`](http://www.haskellstack.org/) to install `gidl`,
then use the default `make` to generate a complete cabal package for
the native Haskell backend.

## Testing

`gidl` generates cabal libraries complete with test suite. The Makefile's `test`
target will install and run the test suite for each generated cabal package.
