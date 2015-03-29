

IVORY_REPO ?= ../../../ivory
GIDL_REPO ?= ../../../gidl

default: smaccm-comm-schema-native

test: smaccm-comm-schema-native-test

smaccm-comm-schema-native: smaccm-comm-schema.idl
	cabal exec gidl -- -b haskell \
		-i smaccm-comm-schema.idl \
		-o smaccm-comm-schema-native \
		-p smaccm-comm-schema-native \
		-n SMACCMPilot.Comm.Native

smaccm-comm-schema-native-test: smaccm-comm-schema-native
	make -C smaccm-comm-schema-native create-sandbox
	make -C smaccm-comm-schema-native
	make -C smaccm-comm-schema-native test

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(GIDL_REPO)
	cabal install gidl


clean:
	-rm -rf smaccm-comm-schema-native

clean-sandbox: clean
	-rm -rf .cabal-sandbox
	-rm -rf cabal.sandbox.config

