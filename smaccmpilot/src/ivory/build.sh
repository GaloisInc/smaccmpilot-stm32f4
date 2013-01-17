#! /bin/sh

TOP=../../..
DSL=$TOP/../dsl
SANDBOX=$DSL/cabal-dev
FILES="pid_stabilize.h pid_stabilize.c"

rm -f $FILES
cabal-dev -s $SANDBOX configure
cabal-dev -s $SANDBOX install
$SANDBOX/bin/smaccm-stabilize
