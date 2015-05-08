#!/bin/sh
cabal run smaccm-comm-client -- \
	--serial=$1 \
	--baud=$2 \
	--conf-file=../smaccm-flight/fmu24.conf \
	--conf-path=../smaccm-flight
