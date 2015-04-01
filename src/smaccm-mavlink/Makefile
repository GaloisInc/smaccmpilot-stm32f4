
IVORY_REPO ?= ../../../ivory
MAVLINK_REPO ?= ../../../mavlink

MAVLINK_MESSAGE_DEFS := $(MAVLINK_REPO)/message_definitions/v1.0
MAVLINK_PYTHON := $(MAVLINK_REPO)/pymavlink/mavlinkv10.py

CABAL_FILE = smaccm-mavlink.cabal

COMMA := ,

MAVLINK_MESSAGE_HS := $(wildcard ./src/SMACCMPilot/Mavlink/Ivory/Messages/*.hs)
MAVLINK_MESSAGE_MODULES := \
    $(patsubst %.hs, SMACCMPilot.Mavlink.Ivory.Messages.%$(COMMA), \
        $(notdir $(MAVLINK_MESSAGE_HS)))

default: build

generate: generate-message-code generate-cabal-file

.PHONY: generate-message-code
generate-message-code:
	-rm ./src/SMACCMPilot/Mavlink/Native/Messages/*.hs
	-rm ./src/SMACCMPilot/Mavlink/Ivory/Messages/*.hs
	python ./generator/pymavlink/generator/smavgen.py \
		--output=./src/SMACCMPilot/Mavlink \
		$(MAVLINK_MESSAGE_DEFS)/smaccmpilot.xml
	@echo "smaccm-mavlink Message modules regenerated. Be sure to remove"
	@echo "deleted files from version control and add new files to version"
	@echo "control."

.PHONY: generate-cabal-file
generate-cabal-file:
	python ./generator/cabal_generator.py --path=./src/SMACCMPilot/Mavlink smaccm-mavlink.cabal

.PHONY: create-sandbox
create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal install --dependencies-only

.PHONY: build
build:
	cabal build

.PHONY: clean-sandbox
clean-sandbox:
	-rm -rf dist
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
