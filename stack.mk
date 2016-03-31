ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

export PATH := $(shell stack path --stack-yaml=$(ROOT_DIR)/gidl-bootstrap.yaml --local-bin-path):$(PATH)

COMM_SCHEMA        := $(ROOT_DIR)/src/smaccm-comm-schema
COMM_SCHEMA_NATIVE := $(COMM_SCHEMA)/smaccm-comm-schema-native
COMM_SCHEMA_TOWER  := $(COMM_SCHEMA)/smaccm-comm-schema-tower

default: $(COMM_SCHEMA_NATIVE) $(COMM_SCHEMA_TOWER)
	stack build

$(COMM_SCHEMA_NATIVE) $(COMM_SCHEMA_TOWER):
	make -C $(ROOT_DIR) gidl-bootstrap
	make -C $(COMM_SCHEMA) smaccm-comm-schema-native smaccm-comm-schema-tower

clean:

distclean: clean
	stack clean

.PHONY: default test clean distclean
