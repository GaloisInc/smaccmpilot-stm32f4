IVORY_FLAGS = --const-fold

.PHONY: test
test: default test-fmu24 test-fmu17 #XXX: needs ev.h px4-sensors-monitor

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
test-fmu24: platform-fmu24/px4-px4io-test-gen
test-fmu24: platform-fmu24/px4-ppm-in-test-gen
test-fmu24: platform-fmu24/px4-adc-test-gen

.PHONY: default
default:
#	stack build ivory-px4-hw --test --no-run-tests


px4-sensors-monitor: default
	stack test ivory-px4-hw:$@-gen --test-arguments '--src-dir=$@ $(IVORY_FLAGS)'
	make -C $@

px4io-packing-test: default
	stack test ivory-px4-hw:px4-$@-gen --test-arguments '--src-dir=$@ $(IVORY_FLAGS)'
	make -C $@

%-gen: default
	stack test ivory-px4-hw:$(@F) --test-arguments '--src-dir=$* --conf-file=$(patsubst platform-%,%,$(*D)).conf $(IVORY_FLAGS)'
	make -C $*

%_gen: default
	stack test ivory-px4-hw:$(@F) --test-arguments '--src-dir=$* --conf-file=$(patsubst platform-%,%,$(*D)).conf $(IVORY_FLAGS)'
	make -C $*

%-clean:
	-rm -rf $*

clean:
	-rm -rf platform-fmu17
	-rm -rf platform-fmu24
	-rm -rf px4-sensors-monitor
	-rm -rf px4io-packing-test
