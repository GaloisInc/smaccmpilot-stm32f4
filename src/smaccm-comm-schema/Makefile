
# Need to use absolute paths so that the IVORY_REPO may be exported for a
# recursive Makefile call

CURRENT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

IVORY_REPO ?= $(CURRENT_DIR)/../../../ivory
GIDL_REPO ?= ../../../gidl

default: smaccm-comm-schema-native
default: smaccm-comm-schema-ivory

test: smaccm-comm-schema-native-test
test: smaccm-comm-schema-ivory-test

.PHONY: smaccm-comm-schema-native
smaccm-comm-schema-native:
	cabal exec gidl -- -b haskell \
		-i smaccm-comm-schema.idl \
		-o smaccm-comm-schema-native \
		-p smaccm-comm-schema-native \
		-n SMACCMPilot.Comm.Native

.PHONY: smaccm-comm-schema-ivory
smaccm-comm-schema-ivory:
	cabal exec gidl -- -b ivory \
		-i smaccm-comm-schema.idl \
		-o smaccm-comm-schema-ivory \
		-p smaccm-comm-schema-ivory \
		-n SMACCMPilot.Comm.Ivory

smaccm-comm-schema-native-test: smaccm-comm-schema-native
	make -C smaccm-comm-schema-native create-sandbox
	make -C smaccm-comm-schema-native
	make -C smaccm-comm-schema-native test

smaccm-comm-schema-ivory-test: smaccm-comm-schema-ivory
	IVORY_REPO=$(IVORY_REPO) make -C smaccm-comm-schema-ivory create-sandbox
	make -C smaccm-comm-schema-ivory
	make -C smaccm-comm-schema-ivory test

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(GIDL_REPO)
	cabal install gidl


clean:
	-rm -rf smaccm-comm-schema-native
	-rm -rf smaccm-comm-schema-ivory

distclean: clean
	-rm -rf dist

clean-sandbox: distclean
	-rm -rf .cabal-sandbox
	-rm -rf cabal.sandbox.config

