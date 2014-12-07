
default: ivory-hxstream ivory-px4-hw

.PHONY: ivory-hxstream
ivory-hxstream:
	make -C src/hx-stream/ivory create-sandbox
	make -C src/hx-stream/ivory
	make -C src/hx-stream/ivory test

.PHONY: ivory-px4-hw
ivory-px4-hw:
	make -C src/ivory-px4-hw create-sandbox
	make -C src/ivory-px4-hw

.PHONY: ivory-px4-tests
ivory-px4-tests:
	make -C src/ivory-px4-tests create-sandbox
	make -C src/ivory-px4-tests
	make -C src/ivory-px4-tests test

.PHONY: commsec-tests
commsec-tests:
	make -C src/crypto/commsec-test create-sandbox
	make -C src/crypto/commsec-test test

.PHONY: smaccm-shared-comm
smaccm-shared-comm:
	make -C src/smaccm-shared-comm create-sandbox
	make -C src/smaccm-shared-comm
