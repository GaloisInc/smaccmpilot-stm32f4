IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32
GEC_REPO ?= ../../../gec
SMACCMPILOT_REPO ?= ..
include ../../Makefile.sandbox

IVORY_FLAGS = --const-fold

.PHONY: test
test: test-fmu17 test-fmu24

.PHONY: test-fmu17
test-fmu17: platform-fmu17/comm-only-test-gen
test-fmu17: platform-fmu17/ppm-test-gen
test-fmu17: platform-fmu17/ui-test-gen

.PHONY: test-fmu24
test-fmu24: platform-fmu24/comm-only-test-gen

%-gen: default
	cabal run $(@F) -- --src-dir=$* --conf-file=$(patsubst platform-%,%,$(*D)).conf $(IVORY_FLAGS)
	make -C $*

%-clean:
	-rm -rf $*

clean:
	-rm -rf platform-fmu17
	-rm -rf platform-fmu24
