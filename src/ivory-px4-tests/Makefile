
IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32

default:
	cabal build

test: ublox-gps-test px4-mpu6k-test px4-ms5611-test px4-hmc5883l-test px4-all-sensors-test px4-copter-motor-test px4-sensors-monitor

.PHONY: ublox-gps-test
ublox-gps-test:
	cabal run ublox-gps-test-gen -- --src-dir=ublox-gps-test
	make -C ublox-gps-test

.PHONY: ublox-gps-test-clean
ublox-gps-test-clean:
	-rm -rf ublox-gps-test

.PHONY: px4-mpu6k-test
px4-mpu6k-test:
	cabal run px4-mpu6k-test-gen -- --src-dir=px4-mpu6k-test
	make -C px4-mpu6k-test

.PHONY: px4-mpu6k-test-clean
px4-mpu6k-test-clean:
	-rm -rf px4-mpu6k-test

.PHONY: px4-ms5611-test
px4-ms5611-test:
	cabal run px4-ms5611-test-gen -- --src-dir=px4-ms5611-test
	make -C px4-ms5611-test

.PHONY: px4-ms5611-test-clean
px4-ms5611-test-clean:
	-rm -rf px4-ms5611-test

.PHONY: px4-hmc5883l-test
px4-hmc5883l-test:
	cabal run px4-hmc5883l-test-gen -- --src-dir=px4-hmc5883l-test
	make -C px4-hmc5883l-test

.PHONY: px4-hmc5883l-test-clean
px4-hmc5883l-test-clean:
	-rm -rf px4-hmc5883l-test

.PHONY: px4-all-sensors-test
px4-all-sensors-test:
	cabal run px4-all-sensors-test-gen -- --src-dir=px4-all-sensors-test
	make -C px4-all-sensors-test

.PHONY: px4-all-sensors-test-clean
px4-all-sensors-test-clean:
	-rm -rf px4-all-sensors-test

.PHONY: px4-copter-motor-test
px4-copter-motor-test:
	cabal run copter-motor-test-gen -- --src-dir=px4-copter-motor-test
	make -C px4-copter-motor-test

.PHONY: px4-copter-motor-test-clean
px4-copter-motor-test-clean:
	-rm -rf px4-copter-motor-test

.PHONY: px4-sensors-monitor
px4-sensors-monitor:
	cabal run px4-sensors-monitor-gen -- --src-dir=px4-sensors-monitor --const-fold
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
	cabal sandbox add-source $(TOWER_REPO)/tower-statemachine
	cabal sandbox add-source $(BSP_REPO)/ivory-freertos-bindings
	cabal sandbox add-source $(BSP_REPO)/tower-freertos-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-tests
	cabal sandbox add-source ../hx-stream/ivory
	cabal sandbox add-source ../ivory-px4-hw
	cabal install --dependencies-only

clean: ublox-gps-test-clean px4-mpu6k-test-clean px4-ms5611-test-clean px4-hmc5883l-test-clean px4-all-sensors-test-clean
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
