# ivory-geo

The `ivory-geo` package provides a functions for calculating distance
and heading from latitude-longitude coordinates.

Presently, `ivory-geo` is not used by other packages in the `smaccmpilot-stm32`
repository.

## Building

The provided `Makefile` has two targets:

- `make create-sandbox` initializes a cabal sandbox in the current directory,
  adds Ivory language sources (see `[../../README.md][d]` for dependency info),
  and installs all dependencies in the sandbox.
- `make` can be run after the sandbox is created. It runs `cabal build` to build
  the `ivory-geo` library.

[d]: https://github.com/GaloisInc/smaccmpilot-stm32f4/blob/master/README.md

## Tests

`ivory-geo` has no tests at this time.

