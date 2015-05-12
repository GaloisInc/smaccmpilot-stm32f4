IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32
GEC_REPO ?= ../../../gec
ODROID_BSP_REPO ?= ../../../tower-camkes-odroid
SMACCMPILOT_REPO ?= ..
include ../../Makefile.sandbox

.PHONY: test
test: smaccm-datalink-ivory-frameloopback-test
test: smaccm-datalink-ivory-commsecloopback-test-stm32

.PHONY: smaccm-datalink-ivory-frameloopback-test
smaccm-datalink-ivory-frameloopback-test:
	cabal run smaccm-datalink-ivory-frameloopback-test-gen -- --src-dir=smaccm-datalink-ivory-frameloopback-test
	make -C smaccm-datalink-ivory-frameloopback-test

.PHONY: smaccm-datalink-ivory-frameloopback-test-clean
smaccm-datalink-ivory-frameloopback-test-clean:
	-rm -rf smaccm-datalink-ivory-frameloopback-test

.PHONY: smaccm-datalink-ivory-commsecloopback-test-stm32
smaccm-datalink-ivory-commsecloopback-test-stm32:
	cabal run smaccm-datalink-ivory-commsecloopback-test-gen-stm32 -- --src-dir=smaccm-datalink-ivory-commsecloopback-test
	make -C smaccm-datalink-ivory-commsecloopback-test

.PHONY: smaccm-datalink-ivory-commsecloopback-test-odroid
smaccm-datalink-ivory-commsecloopback-test-odroid:
	cabal run smaccm-datalink-ivory-commsecloopback-test-gen-odroid -- --src-dir=datalink_aadl_out

.PHONY: smaccm-datalink-ivory-commsecloopback-test-clean
smaccm-datalink-ivory-commsecloopback-test-clean:
	-rm -rf smaccm-datalink-ivory-commsecloopback-test

clean: smaccm-datalink-ivory-frameloopback-test-clean
clean: smaccm-datalink-ivory-commsecloopback-test-clean
