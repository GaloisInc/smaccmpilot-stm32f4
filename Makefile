
default: ivory-hxstream ivory-px4-hw

ivory-hxstream:
	make -C src/hx-stream/ivory create-sandbox
	make -C src/hx-stream/ivory
	make -C src/hx-stream/ivory test

ivory-px4-hw:
	make -C src/ivory-px4-hw create-sandbox
	make -C src/ivory-px4-hw

ivory-px4-tests:
	make -C src/ivory-px4-tests create-sandbox
	make -C src/ivory-px4-tests
	make -C src/ivory-px4-tests test
