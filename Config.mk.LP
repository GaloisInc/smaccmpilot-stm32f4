# Lee Pike's personal Config.mk.
#
# Config.mk --- Example configuration file to build for the STM32F4.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#

# Architecture to compile for (one of mk/arch/*.mk).
CONFIG_ARCH := px4

# GCC prefix for the Cortex-M4 toolchain.
#CONFIG_CORTEX_M4_PREFIX := $(HOME)/local/gcc-arm-none-eabi-4_6-2012q1/bin/arm-none-eabi-
# Or if the toolchain is in your PATH:
CONFIG_CORTEX_M4_PREFIX := $(HOME)/Galois/SMACCM/smaccm-project/OutsideSrcs/gcc-arm-none-eabi-4_7-2012q4/bin/arm-none-eabi-

# Path to cabal-dev sandbox where code generators are built
CONFIG_CABAL_SANDBOX := ../dsl/cabal-dev

# Path to ivory language repository
CONFIG_IVORY_REPOSITORY := ../dsl/ivory

# Path to tower repository
CONFIG_TOWER_REPOSITORY := ../dsl/tower

# Target board (one of mk/board/*.mk).
CONFIG_BOARD := fmu_v17

# Root directory of the FreeRTOS source tree.
CONFIG_FREERTOS_PREFIX := ../../OutsideSrcs/FreeRTOS

# Root directory of the ArduPilot source tree.
# Note: this should be a clone of https://github.com/galoisinc/ardupilot
CONFIG_ARDUPILOT_PREFIX := ../../OutsideSrcs/ardupilot

# Extra arguments to pass to ArduPilot as build flags.
CONFIG_ARDUPILOT_EXTRA_CFLAGS += -DLOGGING_ENABLED=DISABLED
# CONFIG_ARDUPILOT_EXTRA_CFLAGS += -DHIL_MODE=HIL_MODE_ATTITUDE

# Set this variable when using the bootloader on the PX4FMU,
# unset if flashing purely via JTAG.
# CONFIG_PX4FMU_BOOTLOADER := 1

# GCS UART selection (default is "usart1").
# CONFIG_GCS_UART := uart5

# ------------------------------------------------------------------------------
# Optional: for model-checking with CBMC

# add a path to your local installation of the CBMC model-checker
# executable. <http://www.cprover.org/cbmc/> (Please note the license of CBMC.)
CONFIG_CBMC_PREFIX := ../../OutsideSrcs/cbmc/src/cbmc

# location of CBMC driver (cbmc-report)
CONFIG_CBMC_REPORT := $(CONFIG_CABAL_SANDBOX)/bin
