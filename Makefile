include stack.mk

TEST_SUBDIRS = ivory-px4-hw smaccm-commsec smaccm-datalink smaccm-comm-client smaccm-flight

default: smaccm-flight

all: $(TEST_SUBDIRS)

$(TEST_SUBDIRS):
	make -C src/$@ test

.PHONY: $(TEST_SUBDIRS)

# This target bootstraps Gidl if the generated packages do not exist
# yet, then calls the default target to generate them.
.PHONY: gidl-bootstrap
gidl-bootstrap:
	stack --stack-yaml=gidl-bootstrap.yaml install ../gidl --force-dirty || \
        stack --stack-yaml=gidl-bootstrap.yaml install ./gidl --force-dirty

clean:
	make -C src/smaccm-comm-schema clean

TRAVIS_STACK ?= stack
travis-test:
	make gidl-bootstrap
	make -C src/smaccm-comm-schema
	$(TRAVIS_STACK) setup
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make $(TEST_SUBDIRS)
