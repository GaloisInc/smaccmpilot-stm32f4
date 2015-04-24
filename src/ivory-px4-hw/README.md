
# ivory-px4-hw

The `ivory-px4-hw` contains a set of drivers specific to [PX4 Project][px4]
(also called the "Pixhawk Project") flight controller hardware, and a set of
tests for those drivers. These drivers provide support for many of the functions
of the [PX4FMU 1.7][fmu17] and [PX4FMU 2.4 "Pixhawk"][fmu24] flight controllers.

[px4]: https://pixhawk.org
[fmu17]: https://pixhawk.org/modules/px4fmu
[fmu24]: https://pixhawk.org/modules/pixhawk


## Building

The `Makefile` in this directory provides:

- the `create-sandbox` target, which creates a cabal sandbox and installs all
  of `ivory-px4-hw`'s dependencies. This target must be created before any other
  targets.

- the default target, which runs `cabal build`, which serves to type-check the
  library code and executables.

- the `test-fmu17` target, which builds test applications for the PX4FMU 1.7.
  Applications are found in the `platform-fmu17` subdirectory.

- the `test-fmu24` target, which builds test applications for the PX4FMU 2.4
  (Pixhawk). Applications are found in the `platform-fmu24` subdirectory.


## Library

Library source files are found in the `SMACCMPilot/Hardware` subdirectory. The
library contains drivers to support the following hardware:

- Honeywell [HMC5883L][] 3-axis magnetometer, using I2C interface (PX4FMU 1.7
  only)
- ST Micro [LSM303D][] 3-axis magnetometer and 3-axis accelerometer, using SPI
  interface (PX4FMU 2.4 only)
- InvenSense [MPU6000][] 3-axis gyroscope and 3-axis accelerometer, using SPI
  interface
- Measurment Specialties [MS5611][] barometer, using either I2C or SPI interface
- PPM input for radio control system receiver (PX4FMU 1.7 only)
- PWM output for radio control system servos and electronic speed controls
  (PX4FMU 1.7 only)
- UBlox Binary mode GPS, e.g. the [LEA-6H][]-based GPS module included with
  PX4FMU and Pixhawk kits.

[HMC5883L]: http://www51.honeywell.com/aero/common/documents/myaerospacecatalog-documents/Defense_Brochures-documents/HMC5883L_3-Axis_Digital_Compass_IC.pdf
[LSM303D]: http://www.st.com/web/catalog/sense_power/FM89/SC1449/PF253884
[MPU6000]: http://invensense.com/mems/gyro/mpu6050.html
[MS5611]: http://www.meas-spec.com/product/pressure/MS5611-01BA03.aspx
[LEA-6H]: https://pixhawk.org/peripherals/sensors/gps

## Test Applications

Each of these test applications sends output to a serial port. On the PX4FMU
v1.7, we use the UART5 pins on the 15-pin expansion connector. On the PX4FMU
v2.4, we use the port marked TELEM 1.

### px4-baro-test

Tests the barometer (MS5611) driver. Connects to the MS5611 via I2C on the
PX4FMU v1.7, and via SPI on the PX4FMU v2.4. Takes a complete temperature
and pressure reading from the barometer device, and applies calibration
algorithm specified by manufacturer according to the calibration data read
from the device PROM.

### px4-mag-test

Tests the magnetometer driver - HMC5883L via I2C on the PX4FMU v1.7, and
LSM303D via SPI on the PX4FMU v2.4. Reads all three axes of the HMC5883L
magnetometer, and all three axes of both magnetometer and accelerometer
on the LSM303D.

### px4-mpu6k-test

Tests the MPU6000 driver, which works the same way on both PX4FMU v1.7
and v2.4. Reads all three axes of the MPU6000 accelerometer and gyroscope,
as well as the temperature measured on the MPU6000 device.

### px4-ppm-in-test

PX4FMU v1.7 only. Measures PPM signal stream, which encodes 6 to 8 channels of
analog information.

Tested against FrSky D4R-II receiver - the same receiver used by the 3DR Iris
platform.

### ublox-gps-test

Tests UBlox Binary mode GPS driver. GPS should be a standard LEA-6H GPS module
as sold by 3D Robotics or others. GPS must already be configured to speak UBlox
binary mode at 38400 baud. Connect to 5-pin GPS connector on PX4FMU v1.7, and to
6-pin GPS connector on PX4FMU v2.4.

### px4-can-fragment-test

PX4FMU v2.4 only. Tests the CAN bus driver (provided by `ivory-bsp-stm32`
package) with the `tower-hal` library fragmentation protocol, which
breaks up long messages into multiple 8-bit CAN bus messages with sequential CAN
bus ids.

### px4-all-sensors-test

Combines all of the above tests, except the ppm-in and can-fragment tests. This
application reads all sensors continiously. Accelerometer and gyroscope samples
are only sent on the serial port every 4th message, due to serial bandwidth
constraints. Additionally, accelerometer, gyroscope, and magnetometer samples
are sent on the CAN bus using the fragmentation protocol.

Note that, on PX4FMU v2.4, the accelerometer samples from the LSM303D are not
reported over the serial port.

## Sensor Test Client

The sensor test applications all use a binary wire format over a 115200 baud
serial port. The python application `sensorsmonitor.py`, provided in the
`test-client` directory, takes a serial port as an argument, and displays sensor
readings in a human readable format. The user can invoke this program using

```
python test-client/sensorsmonitor.py /PATH/TO/SERIAL/PORT
```

This program requires the user to have the `pyserial` python package installed.

## Motor Test Client

The PWM output driver (typically used to control quadcopter motors) can be
tested with the `copter-motor-test-client`. The user can invoke this program
using

```
cabal run copter-motor-test-client -- --serial=/PATH/TO/SERIAL/PORT
```

The test client provides the user with a prompt. The following commands are
supported:

- `:quit`: exit the program
- `off`: sends a command to turn all motors off (PWM output at minimum value).
- `motor`: takes two integer arguments, a motor number (1 through 4) and a motor
  power (0 through 100). Updates the power of the specified motor to the
  specificed power output, as a percentage. 0% power sets the PWM output to the
  minimum value of 1ms pulse width, 100% sets the PWM output to the maximum
  value of 2ms pulse width.

