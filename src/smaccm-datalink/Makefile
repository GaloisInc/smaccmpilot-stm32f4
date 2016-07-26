.PHONY: test
test: smaccm-datalink-ivory-frameloopback-test
test: smaccm-datalink-ivory-commsecloopback-test-stm32

.PHONY: smaccm-datalink-ivory-frameloopback-test
smaccm-datalink-ivory-frameloopback-test:
	stack test smaccm-datalink:smaccm-datalink-ivory-frameloopback-test-gen --test-arguments '--src-dir=smaccm-datalink-ivory-frameloopback-test'
	make -C smaccm-datalink-ivory-frameloopback-test

.PHONY: smaccm-datalink-ivory-commsecloopback-test-stm32
smaccm-datalink-ivory-commsecloopback-test-stm32:
	stack test smaccm-datalink:smaccm-datalink-ivory-commsecloopback-test-gen-stm32 --test-arguments '--src-dir=smaccm-datalink-ivory-commsecloopback-test'
	make -C smaccm-datalink-ivory-commsecloopback-test

.PHONY: smaccm-datalink-ivory-commsecloopback-test-odroid
smaccm-datalink-ivory-commsecloopback-test-odroid:
	stack test smaccm-datalink:smaccm-datalink-ivory-commsecloopback-test-gen-odroid --test-arguments '--src-dir=datalink_aadl_out'
