IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32
POSIX_REPO ?= ../../../ivory-tower-posix
GEC_REPO ?= ../../../gec
ODROID_BSP_REPO ?= ../../../tower-camkes-odroid
SMACCMPILOT_REPO ?= ..
include ../../Makefile.sandbox

IVORY_FLAGS = --const-fold

.PHONY: test
test: test-fmu24 test-fmu17 px4-sensors-monitor

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
test-fmu24: platform-fmu24/px4-rgbled-test-gen
test-fmu24: platform-fmu24/px4-all-sensors-test-gen

px4-sensors-monitor: default
	cabal run $@-gen -- --src-dir=$@ $(IVORY_FLAGS)
	make -C $@

%-gen: default
	cabal run $(@F) -- --src-dir=$* --conf-file=$(patsubst platform-%,%,$(*D)).conf $(IVORY_FLAGS)
	make -C $*

%-clean:
	-rm -rf $*

clean:
	-rm -rf platform-fmu17
	-rm -rf platform-fmu24
