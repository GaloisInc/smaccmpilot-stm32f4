#!/bin/sh

if [ $# -lt 2 ]; then
	echo "Usage: $0 <serial> <baud> [options]..." >&2
	exit 1
fi

serial=$1
baud=$2
shift 2

cabal run smaccm-comm-client -- \
	--serial=$serial \
	--baud=$baud \
	--conf-file=../smaccm-flight/fmu24.conf \
	--conf-path=../smaccm-flight \
	"$@"
