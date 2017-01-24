ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

export PATH := $(shell stack path --stack-yaml=$(ROOT_DIR)/gidl-bootstrap.yaml --local-bin):$(PATH)

COMM_SCHEMA        := $(ROOT_DIR)/src/smaccm-comm-schema
COMM_SCHEMA_NATIVE := $(COMM_SCHEMA)/smaccm-comm-schema-native
COMM_SCHEMA_TOWER  := $(COMM_SCHEMA)/smaccm-comm-schema-tower
COMM_SCHEMA_ELM    := $(COMM_SCHEMA)/smaccm-comm-schema-elm

default: $(COMM_SCHEMA_TOWER)
	stack build --test --no-run-tests

$(COMM_SCHEMA_TOWER): $(COMM_SCHEMA)/smaccm-comm-schema.gidl
	make -C $(ROOT_DIR) gidl-bootstrap
	make -C $(COMM_SCHEMA)

.PHONY: default
