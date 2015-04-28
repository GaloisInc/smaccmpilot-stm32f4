IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32
GEC_REPO ?= ../../../gec
SMACCMPILOT_REPO ?= ..
include ../../Makefile.sandbox

IVORY_FLAGS ?= --const-fold

test-fmu17: IVORY_FLAGS += --conf-file=fmu17.conf
test-fmu17: SRC_PREFIX = platform-fmu17/
test-fmu17: comm-only-test
test-fmu17: ppm-test

test-fmu24: IVORY_FLAGS += --conf-file=fmu24.conf
test-fmu24: SRC_PREFIX = platform-fmu24/
test-fmu24: comm-only-test


.PHONY: comm-only-test
comm-only-test:
	cabal run comm-only-test-gen -- --src-dir=$(SRC_PREFIX)comm-only-test $(IVORY_FLAGS)
	make -C $(SRC_PREFIX)comm-only-test

.PHONY: comm-only-test-clean
comm-only-test-clean:
	-rm -rf comm-only-test

.PHONY: ppm-test
ppm-test:
	cabal run ppm-test-gen -- --src-dir=$(SRC_PREFIX)ppm-test $(IVORY_FLAGS)
	make -C $(SRC_PREFIX)ppm-test

.PHONY: ppm-test-clean
ppm-test-clean:
	-rm -rf ppm-test

clean: comm-only-test-clean
clean: ppm-test-clean
