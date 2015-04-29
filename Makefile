SUBDIRS = ivory-px4-hw ivory-geo smaccm-commsec smaccm-datalink smaccm-comm-schema smaccm-comm-client smaccm-flight smaccm-ins

default: ivory-px4-hw

all: $(SUBDIRS)

$(SUBDIRS)::
	make -C src/$@ create-sandbox
	make -C src/$@
	make -C src/$@ test

.PHONY: $(SUBDIRS)
