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
test: test-fmu24 test-fmu17 can_proxy_odroid_test smaccmpilot-odroid

.PHONY: test-fmu17
test-fmu17: platform-fmu17/comm-only-test-gen
test-fmu17: platform-fmu17/ppm-test-gen
test-fmu17: platform-fmu17/ui-test-gen
test-fmu17: platform-fmu17/sensors-test-gen

.PHONY: test-fmu24
test-fmu24: platform-fmu24/can-server-test-gen
test-fmu24: platform-fmu24/can-proxy-test-gen
test-fmu24: platform-fmu24/comm-only-test-gen
test-fmu24: platform-fmu24/sensors-test-gen

.PHONY: can_proxy_odroid_test
can_proxy_odroid_test: default
	-rm -rf $@
	-mkdir $@
	cp -r $(ODROID_BSP_REPO)/data/can/* ./$@/
	cp -r tests/odroid-tests/can-proxy-odroid-test-artifacts/* ./$@/
	cabal run can-proxy-odroid-test-gen -- --src-dir=$@ --lib-dir=$@\
    --conf-file=odroid.conf $(IVORY_FLAGS)

.PHONY: smaccmpilot-odroid
smaccmpilot-odroid: default
	-rm -rf smaccmpilot
	-mkdir smaccmpilot
	cp -r $(ODROID_BSP_REPO)/data/can/* ./smaccmpilot/
	cp -r $(ODROID_BSP_REPO)/data/camera_vm/* ./smaccmpilot/
	cp -r tests/odroid-tests/smaccmpilot-odroid-artifacts/* ./smaccmpilot/
	cabal run smaccmpilot-odroid-gen -- --src-dir=smaccmpilot --lib-dir=smaccmpilot\
		--conf-file=odroid.conf $(IVORY_FLAGS)

%-gen: default
	cabal run $(@F) -- --src-dir=$* --conf-file=$(patsubst platform-%,%,$(*D)).conf $(IVORY_FLAGS)
	make -C $*

%-clean:
	-rm -rf $*

clean:
	-rm -rf platform-fmu17
	-rm -rf platform-fmu24
	-rm -rf can_proxy_odroid_test
	-rm -rf smaccmpilot
