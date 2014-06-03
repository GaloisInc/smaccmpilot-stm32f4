#!/bin/sh

cov-configure --comptype gcc --compiler ../../../OutsideSrcs/gcc-arm-none-eabi-4_8-2014q1/bin/arm-none-eabi-gcc

cov-build --dir cov-int/ make PLATFORM=px4fmu17_ioar_freertos TARGET=flight

tar czvf smaccmpilot.tgz cov-int

LOG=`git log -n 1 --pretty=format:"%H"`

curl --data "project=GaloisInc%2Fsmaccmpilot-stm32f4&token=_rpV8hX330d1WDmYUyc7Gg&email=leepike@gmail.com&url=./smaccmpilot.tgz&version=$LOG\
    &description=smaccmpilot source" https://scan.coverity.com/builds?project=GaloisInc%2Fsmaccmpilot-stm32f4