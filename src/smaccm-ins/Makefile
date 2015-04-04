IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32

IVORY_FLAGS ?= --const-fold

default:
	cabal build

test: psas-recording-ivory-test
test: test-fmu24

test-fmu17: IVORY_FLAGS += --conf-file=fmu17.conf
test-fmu17: SRC_PREFIX = platform-fmu17/
test-fmu17: px4-sensor-fusion-test

test-fmu24: IVORY_FLAGS += --conf-file=fmu24.conf
test-fmu24: SRC_PREFIX = platform-fmu24/
test-fmu24: px4-sensor-fusion-test

.PHONY: psas-recording-ivory-test
psas-recording-ivory-test: IVORY_FLAGS += --no-sanity-check
psas-recording-ivory-test:
	cabal run psas-recording-test-gen -- --src-dir=psas-recording-ivory-test $(IVORY_FLAGS)
	make -C psas-recording-ivory-test

.PHONY: psas-recording-ivory-test-clean
psas-recording-ivory-test-clean:
	-rm -rf psas-recording-ivory-test

.PHONY: px4-sensor-fusion-test
px4-sensor-fusion-test:
	cabal run px4-sensor-fusion-test-gen -- --src-dir=$(SRC_PREFIX)px4-sensor-fusion-test --no-sanity-check $(IVORY_FLAGS)
	make -C $(SRC_PREFIX)px4-sensor-fusion-test

.PHONY: px4-sensor-fusion-test-clean
px4-sensor-fusion-test-clean:
	-rm -rf px4-sensor-fusion-test

.PHONY: create-sandbox
create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source $(TOWER_REPO)/tower-config
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-tests
	cabal sandbox add-source $(BSP_REPO)/ivory-freertos-bindings
	cabal sandbox add-source $(BSP_REPO)/tower-freertos-stm32
	cabal sandbox add-source ../smaccm-fragment
	cabal sandbox add-source ../smaccm-commsec
	cabal sandbox add-source ../smaccm-datalink
	cabal sandbox add-source ../ivory-px4-hw
	cabal install --dependencies-only

clean: psas-recording-test-clean

distclean:
	-rm -rf dist

clean-sandbox:
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
