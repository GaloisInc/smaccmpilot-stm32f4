
IVORY_REPO ?= ../../../../ivory

default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-hw
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-serialize
	cabal sandbox add-source $(IVORY_REPO)/ivory-eval
	cabal sandbox add-source $(IVORY_REPO)/ivory-quickcheck
	cabal install --dependencies-only

test-gen:
	cabal run ivory-hxstream-test

test: test-gen
	make -C hxstream-test

test-clean:
	-rm -rf hxstream-test

clean:
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
