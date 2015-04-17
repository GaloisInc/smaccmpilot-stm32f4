
IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32

IVORY_FLAGS = --const-fold

default:
	cabal build

test: smaccm-comm-server-test

.PHONY: smaccm-comm-server-test
smaccm-comm-server-test:
	cabal run smaccm-comm-server-test-gen -- --src-dir=$(SRC_PREFIX)smaccm-comm-server-test $(IVORY_FLAGS)
	make -C $(SRC_PREFIX)smaccm-comm-server-test

.PHONY: smaccm-comm-server-test-clean
smaccm-comm-server-test-clean:
	-rm -rf smaccm-comm-server-test

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal sandbox add-source $(TOWER_REPO)/tower-config
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source $(BSP_REPO)/ivory-freertos-bindings
	cabal sandbox add-source $(BSP_REPO)/tower-freertos-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-tests
	cabal sandbox add-source ../smaccm-fragment
	cabal sandbox add-source ../ivory-px4-hw
	cabal sandbox add-source ../smaccm-commsec
	cabal sandbox add-source ../smaccm-datalink
	make -C ../smaccm-comm-schema create-sandbox
	make -C ../smaccm-comm-schema
	cabal sandbox add-source ../smaccm-comm-schema/smaccm-comm-schema-tower
	cabal install --dependencies-only --max-backjumps=20000

clean: smaccm-comm-server-test-clean

distclean: clean
	-rm -rf dist

clean-sandbox: distclean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
