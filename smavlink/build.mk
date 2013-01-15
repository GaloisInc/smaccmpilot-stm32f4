# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the smavlink library.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, 14 Jan, 2013
#

SMAVLINK_LIB     := libsmavlink.a

SMAVLINK_CFLAGS  += -I$(TOP)/smavlink/include
SMAVLINK_CFLAGS  += -I$(TOP)/include

SMAVLINK_OBJECTS :=    \
  src/channel.o \
  src/crc.o \
  src/send.o

$(eval $(call library,SMAVLINK))

# vim: set ft=make noet ts=2:
