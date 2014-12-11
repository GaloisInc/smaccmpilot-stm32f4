IVORY_REPO ?= ../../../ivory

.PHONY: psas-test
psas-test: cabal.sandbox.config
	cabal run --only codegen -- --src-dir=psas-test --const-fold
	make -C psas-test

create-sandbox: cabal.sandbox.config

.PRECIOUS: cabal.sandbox.config
cabal.sandbox.config:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal install --dependencies-only

clean:
	-rm -rf psas-test dist

clean-sandbox:
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
