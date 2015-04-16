
IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32

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
	cabal sandbox add-source $(TOWER_REPO)/tower-config
	cabal sandbox add-source $(TOWER_REPO)/tower
	cabal sandbox add-source $(BSP_REPO)/ivory-freertos-bindings
	cabal sandbox add-source $(BSP_REPO)/tower-freertos-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-stm32
	cabal sandbox add-source $(BSP_REPO)/ivory-bsp-tests
	cabal sandbox add-source ../ivory-px4-hw
	cabal sandbox add-source ../smaccm-commsec
	cabal sandbox add-source ../smaccm-datalink
	make -C ../smaccm-comm-schema create-sandbox
	make -C ../smaccm-comm-schema
	cabal sandbox add-source ../smaccm-comm-schema/smaccm-comm-schema-native
	cabal install --dependencies-only --max-backjumps=20000

clean:

distclean: clean
	-rm -rf dist

clean-sandbox: distclean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
