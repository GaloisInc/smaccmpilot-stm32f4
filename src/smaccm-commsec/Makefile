
IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower

default:
	cabal build

test: smaccm-commsec-test

.PHONY: smaccm-commsec-test
smaccm-commsec-test:
	cabal run smaccm-commsec-test-gen -- --src-dir=smaccm-commsec-test
	make -C smaccm-commsec-test

.PHONY: smaccm-commsec-test-clean
smaccm-commsec-test-clean:
	-rm -rf smaccm-commsec-test

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal sandbox add-source $(TOWER_REPO)/tower-config
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source ../smaccm-shared-comm
	cabal install --dependencies-only

clean: smaccm-commsec-test-clean
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
