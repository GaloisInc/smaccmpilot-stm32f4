# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for AES-GCM library.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# Written by Lee Pike <leepike@galois.com>, August 2013.
#

CRYPTO_LIB          := commsec.a

CRYPTO_INCLUDES += -I$(TOP)/src/crypto/include

CRYPTO_CFLAGS   += $(CRYPTO_INCLUDES) \
                   -D ARM

CRYPTO_OBJECTS :=  \
  src/aescrypt.o   \
  src/aescrypt.o   \
  src/aeskey.o     \
  src/aestab.o     \
  src/commsec.o    \
  src/gcm.o        \
  src/gf128mul.o   \
  src/gf_convert.o

$(eval $(call library,CRYPTO))

# vim: set ft=make noet ts=2:
