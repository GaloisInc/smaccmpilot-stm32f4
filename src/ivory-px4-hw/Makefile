IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32
GEC_REPO ?= ../../../gec
ODROID_BSP_REPO ?= ../../../tower-camkes-odroid
SMACCMPILOT_REPO ?= ..
include ../../Makefile.sandbox

IVORY_FLAGS = --const-fold

.PHONY: test
test: test-fmu24 test-fmu17

.PHONY: test-fmu17
test-fmu17: platform-fmu17/ublox-gps-test-gen
test-fmu17: platform-fmu17/px4-mpu6k-test-gen
test-fmu17: platform-fmu17/px4-baro-test-gen
test-fmu17: platform-fmu17/px4-mag-test-gen
test-fmu17: platform-fmu17/px4-all-sensors-test-gen
test-fmu17: platform-fmu17/px4-ppm-in-test-gen

.PHONY: test-fmu24
test-fmu24: platform-fmu24/ublox-gps-test-gen
test-fmu24: platform-fmu24/px4-mpu6k-test-gen
test-fmu24: platform-fmu24/px4-baro-test-gen
test-fmu24: platform-fmu24/px4-mag-test-gen
test-fmu24: platform-fmu24/px4-all-sensors-test-gen
# test-fmu24: platform-fmu24/px4-can-fragment-test-gen

%-gen: default
	cabal run $(@F) -- --src-dir=$* --conf-file=$(patsubst platform-%,%,$(*D)).conf $(IVORY_FLAGS)
	make -C $*

%-clean:
	-rm -rf $*

clean:
	-rm -rf platform-fmu17
	-rm -rf platform-fmu24
