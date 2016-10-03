include ../../stack.mk

IVORY_REPO             ?= ../../../ivory
TOWER_REPO             ?= ../../../tower
IVORY_TOWER_STM32_REPO ?= ../../../ivory-tower-stm32

GIDL ?= gidl

default: smaccm-comm-schema-native
default: smaccm-comm-schema-tower

test: smaccm-comm-schema-native-test
test: smaccm-comm-schema-tower-test

.PHONY: smaccm-comm-schema-native
smaccm-comm-schema-native: smaccm-comm-schema.gidl
	$(GIDL) -b haskell-rpc \
	     -i smaccm-comm-schema.gidl \
	     -o smaccm-comm-schema-native \
	     -p smaccm-comm-schema-native \
	     -n SMACCMPilot.Comm.Native

.PHONY: smaccm-comm-schema-tower
smaccm-comm-schema-tower: smaccm-comm-schema.gidl
	$(GIDL) -b tower \
	     -i smaccm-comm-schema.gidl \
	     -o smaccm-comm-schema-tower \
	     -p smaccm-comm-schema-tower \
	     -n SMACCMPilot.Comm \
	     --ivory-repo=$(IVORY_REPO) \
	     --tower-repo=$(TOWER_REPO) \
	     --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_REPO)

smaccm-comm-schema-native-test: smaccm-comm-schema-native
	make -C smaccm-comm-schema-native test

smaccm-comm-schema-tower-test: smaccm-comm-schema-tower
	make -C smaccm-comm-schema-tower test

clean:
	-rm -rf smaccm-comm-schema-native
	-rm -rf smaccm-comm-schema-tower
