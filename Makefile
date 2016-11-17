include stack.mk

SUBDIRS = ivory-px4-hw ivory-geo smaccm-commsec smaccm-datalink smaccm-comm-schema smaccm-comm-client smaccm-flight smaccm-ins

default: ivory-px4-hw

all: $(SUBDIRS)

$(SUBDIRS):
	make -C src/$@ test

.PHONY: $(SUBDIRS)

# This target bootstraps Gidl if the generated packages do not exist
# yet, then calls the default target to generate them.
.PHONY: gidl-bootstrap
gidl-bootstrap:
	stack --stack-yaml=gidl-bootstrap.yaml install ../gidl --force-dirty || \
        stack --stack-yaml=gidl-bootstrap.yaml install ./gidl --force-dirty

clean:
	make -C src/smaccm-comm-schema clean

TRAVIS_STACK ?= stack --no-terminal --system-ghc --skip-ghc-check
travis-test:
	make gidl-bootstrap
	make -C src/smaccm-comm-schema
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make $(SUBDIRS)
