
IVORY_REPO ?= ../../../ivory

IVORY_FLAGS = --const-fold

default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal install --dependencies-only

distclean: clean
	-rm -rf dist

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
