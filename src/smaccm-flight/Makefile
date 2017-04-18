TOWER_CAMKES_ODROID_REPO ?= ../../../tower-camkes-odroid
include ../../stack.mk

IVORY_FLAGS = --const-fold

.PHONY: test
test: test-fmu24 smaccmpilot-odroid-mini # test-fmu17

.PHONY: test-fmu17
test-fmu17: platform-fmu17/comm-only-test-gen
test-fmu17: platform-fmu17/ppm-test-gen
test-fmu17: platform-fmu17/law-test-gen
test-fmu17: platform-fmu17/sensors-test-gen

.PHONY: test-fmu24
test-fmu24: platform-fmu24/can-server-test-gen
test-fmu24: platform-fmu24/can-proxy-test-gen
test-fmu24: platform-fmu24/comm-only-test-gen
test-fmu24: platform-fmu24/sensors-test-gen
test-fmu24: platform-fmu24/ppm-test-gen
test-fmu24: platform-fmu24/law-test-gen
test-fmu24: platform-fmu24/standalone-flight-gen
test-fmu24: platform-fmu24/lidar-test-gen
# disabled so we can keep the default config internal sensors only
#test-fmu24: platform-fmu24/hmc5883l-test-gen
test-fmu24: platform-fmu24/esccal-test-gen

.PHONY: test-fmu24-echronos
test-fmu24-echronos: platform-fmu24/sensors_aadl_test_gen
test-fmu24-echronos: platform-fmu24/standalone_flight_echronos_gen

.PHONY: flight
flight: platform-fmu24/standalone-flight-gen

.PHONY: upload-flight
upload-flight:
	make -C platform-fmu24/standalone-flight upload

.PHONY: flight_echronos
flight_echronos: platform-fmu24/standalone_flight_echronos_gen

.PHONY: upload_flight_echronos
upload_flight_echronos:
	make -C platform-fmu24/standalone_flight_echronos upload

.PHONY: can_proxy_odroid_test
can_proxy_odroid_test: default
	-rm -rf $@
	-mkdir $@
	cp -r $(TOWER_CAMKES_ODROID_REPO)/data/uart/* ./$@/
	cp -r $(TOWER_CAMKES_ODROID_REPO)/data/can/* ./$@/
	cp -r tests/odroid-tests/can-proxy-odroid-test-artifacts/* ./$@/
	stack exec -- can-proxy-odroid-test-gen \
			--src-dir=$@ --lib-dir=$@ \
			--conf-file=odroid.conf $(IVORY_FLAGS)

.PHONY: smaccmpilot-odroid
smaccmpilot-odroid: default
	-rm -rf smaccmpilot
	-mkdir smaccmpilot
	cp -r $(TOWER_CAMKES_ODROID_REPO)/data/uart/* ./smaccmpilot/
	cp -r $(TOWER_CAMKES_ODROID_REPO)/data/can/* ./smaccmpilot/
	cp -r $(TOWER_CAMKES_ODROID_REPO)/data/camera_vm/* ./smaccmpilot/
	cp -r tests/odroid-tests/smaccmpilot-odroid-artifacts/* ./smaccmpilot/
	stack exec -- smaccmpilot-odroid-gen \
			--src-dir=smaccmpilot --lib-dir=smaccmpilot \
			--conf-file=odroid.conf $(IVORY_FLAGS)

.PHONY: smaccmpilot-odroid-mini
smaccmpilot-odroid-mini: default
	-rm -rf smaccmpilot-mini
	-mkdir smaccmpilot-mini
	stack exec -- smaccmpilot-odroid-mini-gen \
			--src-dir=smaccmpilot-mini --lib-dir=smaccmpilot-mini \
			--conf-file=odroid-mini.conf $(IVORY_FLAGS)

%-gen: default
	-rm -rf $*
	stack exec -- $(@F) --src-dir=$* \
			    --conf-file=$(patsubst platform-%,%,$(*D)).conf \
			    $(IVORY_FLAGS)
	make -C $*

%-clean:
	-rm -rf $*

%_gen: default
	-rm -rf $*
	stack exec -- $(@F) --src-dir=$* \
		      --conf-file=$(patsubst platform-%,%,$(*D)).conf \
					$(IVORY_FLAGS)
	make -C $*

%_clean:
	-rm -rf $*

clean:
	-rm -rf platform-fmu17
	-rm -rf platform-fmu24
	-rm -rf can_proxy_odroid_test
	-rm -rf smaccmpilot
	-rm -rf smaccmpilot-mini
