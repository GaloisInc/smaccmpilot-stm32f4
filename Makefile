
default: ivory-px4-hw
all: ivory-px4-hw
all: smaccm-commsec
all: smaccm-datalink
all: smaccm-mavlink
all: smaccm-ins

.PHONY: ivory-px4-hw
ivory-px4-hw:
	make -C src/ivory-px4-hw create-sandbox
	make -C src/ivory-px4-hw
	make -C src/ivory-px4-hw test

.PHONY: smaccm-commsec
smaccm-commsec:
	make -C src/smaccm-commsec create-sandbox
	make -C src/smaccm-commsec
	make -C src/smaccm-commsec test

.PHONY: smaccm-datalink
smaccm-datalink:
	make -C src/smaccm-datalink create-sandbox
	make -C src/smaccm-datalink
	make -C src/smaccm-datalink test

.PHONY: smaccm-mavlink
smaccm-mavlink:
	make -C src/smaccm-mavlink create-sandbox
	make -C src/smaccm-mavlink

.PHONY: smaccm-ins
smaccm-ins:
	make -C src/smaccm-ins create-sandbox
	make -C src/smaccm-ins
	make -C src/smaccm-ins test
