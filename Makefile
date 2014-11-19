
default: ivory-px4-tests

ivory-hxstream:
	make -C src/hxstream/ivory create-sandbox
	make -C src/hxstream/ivory
	make -C src/hxstream/ivory test

ivory-px4-hw:
	make -C src/ivory-px4-hw create-sandbox
	make -C src/ivory-px4-hw

ivory-px4-tests:
	make -C src/ivory-px4-tests create-sandbox
	make -C src/ivory-px4-tests
	make -C src/ivory-px4-tests test
