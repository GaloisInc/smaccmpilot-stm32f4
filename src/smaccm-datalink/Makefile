
IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32

default:
	cabal build

.PHONY: test
test: smaccm-datalink-ivory-test

.PHONY: smaccm-datalink-ivory-test
smaccm-datalink-ivory-test:
	cabal run smaccm-datalink-ivory-test-gen -- --src-dir=smaccm-datalink-ivory-test
	make -C smaccm-datalink-ivory-test

.PHONY: smaccm-datalink-ivory-test-clean
smaccm-datalink-ivory-test-clean:
	-rm -rf smaccm-datalink-ivory-test


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
	cabal sandbox add-source $(TOWER_REPO)/tower-statemachine
	cabal sandbox add-source $(BSP_REPO)/ivory-freertos-bindings
	cabal sandbox add-source $(BSP_REPO)/tower-freertos-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-tests
	cabal sandbox add-source ../hx-stream/ivory
	cabal sandbox add-source ../hx-stream/hs
	cabal sandbox add-source ../ivory-px4-hw
	cabal sandbox add-source ../ivory-px4-tests
	cabal sandbox add-source ../smaccm-commsec
	cabal install --dependencies-only

clean: smaccm-datalink-ivory-test-clean
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
