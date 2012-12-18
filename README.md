
Toolchain Setup
===============

## Installing the launchpad arm toolchain

 1. Download the toolchain from:
    [https://launchpad.net/gcc-arm-embedded/4.6/4.6-2012-q1-update/+download/gcc-arm-none-eabi-4_6-2012q1-20120316.tar.bz2]
 2. Unpack the toolchain somewhere, and make sure that it's in your PATH.


## Install OpenOCD

 1. Download a new release of OpenOCD, this was tested with 0.6.1
 2. Configure and install OpenOCD with:
```shell
$ tar -zxvf openocd-0.6.1.tar.gz
$ cd openocd-0.6.1
# NOTE: many of these flags are not necessary
$ ./configure --enable-ftdi --enable-ft2232_libftdi --enable-stlink \
  --enable-jlink --enable-amtjtagaccel --enable-buspirate
$ make
% make install
```
