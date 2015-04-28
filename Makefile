SUBDIRS = ivory-px4-hw ivory-geo smaccm-commsec smaccm-datalink smaccm-comm-schema smaccm-comm-client smaccm-flight smaccm-ins

default: ivory-px4-hw

all: $(SUBDIRS)

$(SUBDIRS)::
	make -C src/$@ create-sandbox
	make -C src/$@
	make -C src/$@ test

smaccm-flight::
	make -C src/smaccm-flight test-fmu17
	make -C src/smaccm-flight test-fmu24

.PHONY: $(SUBDIRS)
