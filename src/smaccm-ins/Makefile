IVORY_REPO ?= ../../../ivory

.PHONY: psas
psas: cabal.sandbox.config
	cabal install --dependencies-only
	cabal run --only codegen -- --src-dir=psas --const-fold
	make -C psas

create-sandbox: cabal.sandbox.config

.PRECIOUS: cabal.sandbox.config
cabal.sandbox.config:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib

clean:
	-rm -rf psas dist

clean-sandbox:
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
