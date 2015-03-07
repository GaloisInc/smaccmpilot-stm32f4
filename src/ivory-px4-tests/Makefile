
IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32

IVORY_FLAGS = --const-fold

default:
	cabal build

test: ublox-gps-test
test: px4-mpu6k-test
test: px4-baro-test
test: px4-mag-test
test: px4-ppm-in-test
test: px4-all-sensors-test
test: px4-copter-motor-test
test: px4-sensor-fusion-test
test: px4-fragment-test

.PHONY: ublox-gps-test
ublox-gps-test:
	cabal run ublox-gps-test-gen -- --src-dir=ublox-gps-test $(IVORY_FLAGS)
	make -C ublox-gps-test

.PHONY: ublox-gps-test-clean
ublox-gps-test-clean:
	-rm -rf ublox-gps-test

.PHONY: px4-mpu6k-test
px4-mpu6k-test:
	cabal run px4-mpu6k-test-gen -- --src-dir=px4-mpu6k-test $(IVORY_FLAGS)
	make -C px4-mpu6k-test

.PHONY: px4-mpu6k-test-clean
px4-mpu6k-test-clean:
	-rm -rf px4-mpu6k-test

.PHONY: px4-baro-test
px4-baro-test:
	cabal run px4-baro-test-gen -- --src-dir=px4-baro-test $(IVORY_FLAGS)
	make -C px4-baro-test

.PHONY: px4-baro-test-clean
px4-baro-test-clean:
	-rm -rf px4-baro-test

.PHONY: px4-mag-test
px4-mag-test:
	cabal run px4-mag-test-gen -- --src-dir=px4-mag-test $(IVORY_FLAGS)
	make -C px4-mag-test

.PHONY: px4-mag-test-clean
px4-mag-test-clean:
	-rm -rf px4-mag-test

.PHONY: px4-all-sensors-test
px4-all-sensors-test:
	cabal run px4-all-sensors-test-gen -- --src-dir=px4-all-sensors-test $(IVORY_FLAGS)
	make -C px4-all-sensors-test

.PHONY: px4-all-sensors-test-clean
px4-all-sensors-test-clean:
	-rm -rf px4-all-sensors-test

.PHONY: px4-copter-motor-test
px4-copter-motor-test:
	cabal run copter-motor-test-gen -- --src-dir=px4-copter-motor-test $(IVORY_FLAGS)
	make -C px4-copter-motor-test

.PHONY: px4-copter-motor-test-clean
px4-copter-motor-test-clean:
	-rm -rf px4-copter-motor-test

.PHONY: px4-ppm-in-test
px4-ppm-in-test:
	cabal run px4-ppm-in-test-gen -- --src-dir=px4-ppm-in-test $(IVORY_FLAGS)
	make -C px4-ppm-in-test

.PHONY: px4-ppm-in-test-clean
px4-ppm-in-test-clean:
	-rm -rf px4-ppm-in-test

.PHONY: px4-sensor-fusion-test
px4-sensor-fusion-test:
	cabal run px4-sensor-fusion-test-gen -- --src-dir=px4-sensor-fusion-test --no-sanity-check $(IVORY_FLAGS)
	make -C px4-sensor-fusion-test

.PHONY: px4-sensor-fusion-test-clean
px4-sensor-fusion-test-clean:
	-rm -rf px4-sensor-fusion-test

.PHONY: px4-fragment-test
px4-fragment-test:
	cabal run px4-fragment-test-gen -- --src-dir=px4-fragment-test --no-sanity-check $(IVORY_FLAGS)
	make -C px4-fragment-test

.PHONY: px4-fragment-test-clean
px4-fragment-test-clean:
	-rm -rf px4-fragment-test

.PHONY: px4-sensors-monitor
px4-sensors-monitor:
	cabal run px4-sensors-monitor-gen -- --src-dir=px4-sensors-monitor $(IVORY_FLAGS)
	make -C px4-sensors-monitor

.PHONY: px4-sensors-monitor-clean
px4-sensors-monitor-clean:
	-rm -rf px4-sensors-monitor

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal sandbox add-source $(IVORY_REPO)/ivory-eval
	cabal sandbox add-source $(TOWER_REPO)/tower-config
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source $(BSP_REPO)/ivory-freertos-bindings
	cabal sandbox add-source $(BSP_REPO)/tower-freertos-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-tests
	cabal sandbox add-source ../smaccm-commsec
	cabal sandbox add-source ../smaccm-datalink
	cabal sandbox add-source ../smaccm-fragment
	cabal sandbox add-source ../smaccm-ins
	cabal sandbox add-source ../smaccm-mavlink
	cabal sandbox add-source ../ivory-px4-hw
	cabal install --dependencies-only

clean: ublox-gps-test-clean
clean: px4-mpu6k-test-clean
clean: px4-baro-test-clean
clean: px4-mag-test-clean
clean: px4-ppm-in-test-clean
clean: px4-copter-motor-test-clean
clean: px4-all-sensors-test-clean
clean: px4-sensor-fusion-test-clean
clean: px4-fragment-test-clean
clean: px4-sensors-monitor-clean

clean-sandbox: clean
	-rm -rf dist
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
