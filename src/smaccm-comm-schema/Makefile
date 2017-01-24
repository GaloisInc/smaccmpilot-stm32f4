IVORY_REPO             ?= ../../../ivory
TOWER_REPO             ?= ../../../tower
IVORY_TOWER_STM32_REPO ?= ../../../ivory-tower-stm32

GIDL ?= gidl

default: smaccm-comm-schema-native
default: smaccm-comm-schema-tower
default: smaccm-comm-schema-elm

test: smaccm-comm-schema-native-test
test: smaccm-comm-schema-tower-test
test: smaccm-comm-schema-elm-test

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

.PHONY: smaccm-comm-schema-elm
smaccm-comm-schema-elm: smaccm-comm-schema.gidl
	$(GIDL) -b elm \
	     -i smaccm-comm-schema.gidl \
	     -o smaccm-comm-schema-elm \
	     -p smaccm-comm-schema-elm \
	     -n SMACCMPilot.Comm

smaccm-comm-schema-native-test: smaccm-comm-schema-native
	make -C smaccm-comm-schema-native test

smaccm-comm-schema-tower-test: smaccm-comm-schema-tower
	make -C smaccm-comm-schema-tower test

smaccm-comm-schema-elm-test: smaccm-comm-schema-elm
	# no tests yet

clean:
	-rm -rf smaccm-comm-schema-native
	-rm -rf smaccm-comm-schema-tower
	-rm -rf smaccm-comm-schema-elm
