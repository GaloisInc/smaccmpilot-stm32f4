# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# running cppcheck on sources
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Lee Pike <leepike@galois.com>

# XXX We'd like to generalize this for multiple platforms, but it's clunky as a
# phony target.

# We'll leave these out so we don't evaluate the the crypto headers.
# $(FLIGHT_INCLUDES)

# -j4			\ use 4 cores
# --std=c99      	\ check against c99 standard
# --error-exitcode=1	\ exit with 1 if an error is
#                         found (analysis is sound)
.PHONY: cpp-check
cpp-check:
	$(CONFIG_CPP_CHECK_PREFIX)/cppcheck \
          -j4 \
          --std=c99 \
          --error-exitcode=1 \
          $(GEN_DIR)

# vim: set ft=make noet ts=2:

