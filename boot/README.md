# PX4 Bootloader

The contents of this directory come from the PX4 project Bootloader
repository:

http://github.com/PX4/Bootloader

Two python scripts are included to prepare image files and upload images,
and a binary 

# Dependencies

You will need Python 2.6 or better, and the `pyserial` Python module.

# Contents

## Image Creation

The script `px_mkfw.py` produces a `.px4` image file from a `.bin` binary file.
PX4 images are annotated with metadata which the uploader utility uses to ensure
compatibility with the connected board.

## Uploading

The script `px_uploader.py` takes a set of serial port names and a `.px4` image
file, and attempts to upload an image to a connected PX4 board.

## Bootloader Image

The PX4FMU v1.x bootloader binary `px4fmu_bl.elf` is provided for convenience.
The PX4FMU comes with this bootloader from the 3DR factory, but it will be wiped
off if you write a non-bootloader image using JTAG/SWD.

# LICENSE

The source files in this directory are copyright (C) 2012 PX4 Development Team.
See file contents for details.

