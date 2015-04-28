IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
include ../../Makefile.sandbox

.PHONY: test
test: smaccm-commsec-ivory-test

.PHONY: smaccm-commsec-ivory-test
smaccm-commsec-ivory-test:
	cabal run smaccm-commsec-ivory-test-gen -- --src-dir=smaccm-commsec-ivory-test
	make -C smaccm-commsec-ivory-test
	./smaccm-commsec-ivory-test/test

.PHONY: smaccm-commsec-ivory-test-clean
smaccm-commsec-ivory-test-clean:
	-rm -rf smaccm-commsec-ivory-test

.PHONY: smaccm-commsec-native-test
smaccm-commsec-native-test:
	cabal run smaccm-commsec-native-test

clean: smaccm-commsec-ivory-test-clean
