# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#

IVORY_PKG_OPEN407TOY_GEN_SYMS    := true

$(eval $(call when_platforms,open407vc,tower_pkg,IVORY_PKG_OPEN407TOY,bsp-open407-toy-gen))

BSP_OPEN407TOY_IMG          := bsp-open407-toy
BSP_OPEN407TOY_OBJECTS      := main.o
BSP_OPEN407TOY_REAL_OBJECTS += $(IVORY_PKG_OPEN407TOY_OBJECTS)
BSP_OPEN407TOY_LIBRARIES    += libFreeRTOS.a

BSP_OPEN407TOY_INCLUDES     += $(FREERTOS_CFLAGS)
BSP_OPEN407TOY_INCLUDES     += -I$(TOP)/src/bsp/include
BSP_OPEN407TOY_INCLUDES     += $(IVORY_PKG_OPEN407TOY_CFLAGS)
BSP_OPEN407TOY_CFLAGS       += -O2 $(BSP_OPEN407TOY_INCLUDES)

BSP_OPEN407TOY_LIBRARIES    += libFreeRTOS.a

$(eval $(call when_platforms,open407vc,cbmc_pkg,BSP_OPEN407TOY,IVORY_PKG_OPEN407TOY))

$(eval $(call when_platforms,open407vc,image,BSP_OPEN407TOY))

