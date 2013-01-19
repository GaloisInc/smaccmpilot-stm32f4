#! /bin/sh

TOP=..
DSL=$TOP/../dsl
SANDBOX=$DSL/cabal-dev

cabal-dev -s $SANDBOX ghci
