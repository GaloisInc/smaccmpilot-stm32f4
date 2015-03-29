
default: ivory-px4-hw
all: ivory-px4-hw
all: ivory-geo
all: smaccm-commsec
all: smaccm-datalink
all: smaccm-ins

.PHONY: ivory-px4-hw
ivory-px4-hw:
	make -C src/ivory-px4-hw create-sandbox
	make -C src/ivory-px4-hw
	make -C src/ivory-px4-hw test-fmu17
	make -C src/ivory-px4-hw test-fmu24

.PHONY: ivory-geo
ivory-geo:
	make -C src/ivory-geo create-sandbox
	make -C src/ivory-geo

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

.PHONY: smaccm-comm-schema
smaccm-comm-schema:
	make -C src/smaccm-comm-schema create-sandbox
	make -C src/smaccm-comm-schema
	make -C src/smaccm-comm-schema test

.PHONY: smaccm-ins
smaccm-ins:
	make -C src/smaccm-ins create-sandbox
	make -C src/smaccm-ins
	make -C src/smaccm-ins test

.PHONY: smaccm-params
smaccm-params:
	make -C src/smaccm-params create-sandbox
	make -C src/smaccm-params
	# make -C src/smaccm-params test
