
Build Instructions
==================

## Cortex-M4 Toolchain

 1. Download the toolchain from:
    [https://launchpad.net/gcc-arm-embedded/4.6/4.6-2012-q1-update/+download/gcc-arm-none-eabi-4_6-2012q1-20120316.tar.bz2]
 2. Unpack the toolchain somewhere, and make sure that it's in your PATH.

## FreeRTOS

 1. Download the FreeRTOS distribution from:
    [http://sourceforge.net/projects/freertos/files/]
 2. Unpack the ZIP file anywhere.  We will refer to the root directory of
    the unzipped source tree during build configuration.

## Install OpenOCD (needed for STM32F4 Discovery board)

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

## Configuration File

  1. Copy `Config.mk.example` to `Config.mk`.  Open this file in a text editor.
  2. Set `CONFIG_CORTEX_M4_PREFIX` to the location of the Cortex-M4 toolchain.
  3. Set `CONFIG_FREERTOS_PREFIX` to the location of the FreeRTOS source.
  4. Set `CONFIG_BOARD` to `px4` for the PX4FMU, or `stm32f4-discovery`.

## Compiling

  1. Simply run `make` from the top-level directory.  You can also specify
     a specific target such as `libhwf4.a`.
  2. Build output is placed in the `build` directory.  The test program images
     are in the `build/$(CONFIG_ARCH)/$(CONFIG_BOARD)/img` directory.

## Flashing the PX4FMU Board

  1. This process uses the Black Magic Probe JTAG adapter on a Linux system.  This
     will show up as a serial device, such as `/dev/ttyACM0`.
  2. Start GDB: `arm-none-eabi-gdb`
  3. Connect to the JTAG adapter: `target extended-remote /dev/ttyACM0`
  4. Scan the single-wire debug adapters: `mon swdp_scan`
  5. This should display a single result, such as: `1 STM32F4xx`.
  6. Attach to this target: `attach 1`
  7. Load the image to flash: `file build/cortex-m4/px4/img/ledtest`
  8. Flash the image to the device: `load`
  9. Execute the firmware: `run`
  10. Interrupt the debugger with Control-C to reflash by running `load` and `run` again.
