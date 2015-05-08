# Need to use absolute paths so that the IVORY_REPO may be exported for a
# recursive Makefile call

CURRENT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

IVORY_REPO ?= $(CURRENT_DIR)/../../../ivory
TOWER_REPO ?= $(CURRENT_DIR)/../../../tower
BSP_REPO ?= $(CURRENT_DIR)/../../../ivory-tower-stm32
GIDL_REPO ?= $(CURRENT_DIR)/../../../gidl
SCARGOT_REPO ?= $(CURRENT_DIR)/../../../s-cargot
SMACCMPILOT_REPO ?= ..

SANDBOX_INSTALL = gidl
include ../../Makefile.sandbox

default: smaccm-comm-schema-native
default: smaccm-comm-schema-tower

test: smaccm-comm-schema-native-test
test: smaccm-comm-schema-tower-test

.PHONY: smaccm-comm-schema-native
smaccm-comm-schema-native:
	cabal exec gidl -- -b haskell-rpc\
		-i smaccm-comm-schema.gidl \
		-o smaccm-comm-schema-native \
		-p smaccm-comm-schema-native \
		-n SMACCMPilot.Comm.Native

.PHONY: smaccm-comm-schema-tower
smaccm-comm-schema-tower:
	cabal exec gidl -- -b tower \
		-i smaccm-comm-schema.gidl \
		-o smaccm-comm-schema-tower \
		-p smaccm-comm-schema-tower \
		-n SMACCMPilot.Comm

smaccm-comm-schema-native-test: smaccm-comm-schema-native
	make -C smaccm-comm-schema-native create-sandbox
	make -C smaccm-comm-schema-native
	make -C smaccm-comm-schema-native test

smaccm-comm-schema-tower-test: smaccm-comm-schema-tower
	IVORY_REPO=$(IVORY_REPO) \
	TOWER_REPO=$(TOWER_REPO) \
	BSP_REPO=$(BSP_REPO) \
		make -C smaccm-comm-schema-tower create-sandbox
	make -C smaccm-comm-schema-tower
	make -C smaccm-comm-schema-tower test

clean:
	-rm -rf smaccm-comm-schema-native
	-rm -rf smaccm-comm-schema-tower
