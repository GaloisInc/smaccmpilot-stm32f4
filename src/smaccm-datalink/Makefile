include ../../stack.mk

.PHONY: test
test: smaccm-datalink-ivory-frameloopback-test
test: smaccm-datalink-ivory-commsecloopback-test-stm32

.PHONY: smaccm-datalink-ivory-frameloopback-test
smaccm-datalink-ivory-frameloopback-test:
	stack build . --exec 'smaccm-datalink-ivory-frameloopback-test-gen --src-dir=smaccm-datalink-ivory-frameloopback-test'
	make -C smaccm-datalink-ivory-frameloopback-test

.PHONY: smaccm-datalink-ivory-frameloopback-test-clean
smaccm-datalink-ivory-frameloopback-test-clean:
	-rm -rf smaccm-datalink-ivory-frameloopback-test

.PHONY: smaccm-datalink-ivory-commsecloopback-test-stm32
smaccm-datalink-ivory-commsecloopback-test-stm32:
	stack build . --exec 'smaccm-datalink-ivory-commsecloopback-test-gen-stm32 --src-dir=smaccm-datalink-ivory-commsecloopback-test'
	make -C smaccm-datalink-ivory-commsecloopback-test

.PHONY: smaccm-datalink-ivory-commsecloopback-test-odroid
smaccm-datalink-ivory-commsecloopback-test-odroid:
	stack build . --exec 'smaccm-datalink-ivory-commsecloopback-test-gen-odroid --src-dir=datalink_aadl_out'

.PHONY: smaccm-datalink-ivory-commsecloopback-test-clean
smaccm-datalink-ivory-commsecloopback-test-clean:
	-rm -rf smaccm-datalink-ivory-commsecloopback-test

clean: smaccm-datalink-ivory-frameloopback-test-clean
clean: smaccm-datalink-ivory-commsecloopback-test-clean
