# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
# vim: set ft=make noet ts=2:

RTV_DIR=apps/sample-rtv-task
RTV_GENERATEDDIR=$(RTV_DIR)/generated
RTV_GRAPHS_DIR=$(RTV_DIR)/graphs

RTV_SRCDIR=$(RTV_GENERATEDDIR)/src
RTV_INCDIR=$(RTV_GENERATEDDIR)/include/generated

RTV_GENERATOR_EXE=$(CONFIG_CABAL_SANDBOX)/bin/sample-rtv-task-checker-gen

RTV_IVORY_OPTS=--const-fold --overflow --div-zero
# A little too noisy: --fp-check

RTV_GENERATED_DEP=$(RTV_GENERATEDDIR)/dep.mk

include $(RTV_GENERATED_DEP)

# ------------------------------------------------------------------------------

# Generate the srcs and headers.
RTV += rtvtest-build
.PHONY: rtvtest-build
rtvtest-build: $(RTV_GENERATED_HEADERS) $(RTV_GENERATED_SOURCES)

# This is the first build.
$(RTV_GENERATED_DEP): $(TWRTEST_GENERATOR_EXE)
	mkdir -p $(RTV_GRAPHS_DIR)
	mkdir -p $(RTV_SRCDIR)
	mkdir -p $(RTV_INCDIR)
	$(RTV_GENERATOR_EXE) \
	--src-dir=$(RTV_SRCDIR) \
	--include-dir=$(RTV_INCDIR) \
	--deps=$(RTV_GENERATED_DEP) \
	--dep-prefix=RTV_GENERATED \
	$(RTV_IVORY_OPTS)

$(RTV_GENERATED_HEADERS) $(RTV_GENERATED_SOURCES): $(RTV_GENERATED_DEP)
	$(RTV_GENERATOR_EXE) $(TWRTEST_GENERATOR_EXE) \
	--src-dir=$(RTV_SRCDIR) \
	--include-dir=$(RTV_INCDIR) \
	$(RTV_IVORY_OPTS)

CLEAN     += $(RTV_GENERATED_DEP)
# use wildcard, not the dep file, to clean subdirs, because if dep file
# doesn't exist we won't get a proper clean.
CLEAN     += $(wildcard $(RTV_SRCDIR)/*.c)
CLEAN     += $(wildcard $(RTV_INCDIR)/*.h)

CLEAN     += $(RTV_GRAPHS_DIR)

include apps/sample-rtv-task/app.mk
