/*
 * main.cpp --- AP_HAL SPI driver test.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>

#if CONFIG_HAL_BOARD == HAL_BOARD_SMACCM
# include <AP_HAL_SMACCM.h>
#else
# error "Unsupported CONFIG_HAL_BOARD type."
#endif

using namespace AP_HAL;

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle htask;

/// Read a 16-bit integer in big endian order from a buffer.
uint16_t get_be16(const uint8_t *buf)
{
  return ((uint16_t)(buf[0]) << 8) |
         ((uint16_t)(buf[1]) << 0);
}

/// Read an 8-bit register from the MPU6000.
uint8_t mpu6000_read(uint8_t addr)
{
  SPIDeviceDriver *dev = hal.spi->device(SPIDevice_MPU6000);
  uint8_t buf[2];

  buf[0] = addr | 0x80;
  buf[1] = 0x00;

  dev->transaction(buf, buf, sizeof(buf));
  return buf[1];
}

/// Read multiple registers from the MPU6000.
///
/// This reads 'len - 1' registers into 'buf + 1'.  The byte at the
/// beginning is don't care and required for SPI communication.
void mpu6000_read_burst(uint8_t start_addr, uint8_t *buf, size_t len)
{
  SPIDeviceDriver *dev = hal.spi->device(SPIDevice_MPU6000);

  buf[0] = start_addr | 0x80;
  dev->transaction(buf, buf, len);
}

/// Write an 8-bit register to the MPU6000.
void mpu6000_write(uint8_t addr, uint8_t value)
{
  SPIDeviceDriver *dev = hal.spi->device(SPIDevice_MPU6000);
  uint8_t buf[2];

  buf[0] = addr;
  buf[1] = value;

  dev->transaction(buf, buf, sizeof(buf));
}

/// Initialize the MPU6000.
void mpu6000_init()
{
  mpu6000_write(0x6A, 0x10);    // disable I2C interface
  mpu6000_write(0x6B, 0x00);    // wake up, use internal osc.
  mpu6000_write(0x1B, 0x18);    // gyro at +/- 2000 deg/s

  uint8_t id = mpu6000_read(0x75);
  hal.console->printf("MPU6000 Device ID: 0x%02x\r\n", id);
}

/** A raw sample of all sensors on the MPU6000. */
typedef struct {
  int16_t accel_x;
  int16_t accel_y;
  int16_t accel_z;
  int16_t temp;
  int16_t gyro_raw_x;
  int16_t gyro_raw_y;
  int16_t gyro_raw_z;
  float   gyro_x;
  float   gyro_y;
  float   gyro_z;
} mpu6000_sample_t;

/// Read a raw sensor sample from the MPU6000.
void mpu6000_read_sample(mpu6000_sample_t *sample)
{
  uint8_t buf[15];

  memset(buf, 0, sizeof(buf));
  mpu6000_read_burst(0x3B, buf, sizeof(buf));

  sample->accel_x    = (int16_t)get_be16(&buf[1]);
  sample->accel_y    = (int16_t)get_be16(&buf[3]);
  sample->accel_z    = (int16_t)get_be16(&buf[5]);
  sample->temp       = (int16_t)get_be16(&buf[7]);

  sample->gyro_raw_x = (int16_t)get_be16(&buf[9]);
  sample->gyro_raw_y = (int16_t)get_be16(&buf[11]);
  sample->gyro_raw_z = (int16_t)get_be16(&buf[13]);

  sample->gyro_x = (float)sample->gyro_raw_x / 16.4;
  sample->gyro_y = (float)sample->gyro_raw_y / 16.4;
  sample->gyro_z = (float)sample->gyro_raw_z / 16.4;
}

/// Print an MPU6000 sample to the debug console.
void mpu6000_print_sample(const mpu6000_sample_t *sample)
{
  hal.console->printf("  mpu6000: acc %d %d %d  temp %d  gyro %.3f %.3f %.3f\r\n",
                      sample->accel_x, sample->accel_y, sample->accel_z,
                      sample->temp, sample->gyro_x, sample->gyro_y, sample->gyro_z);
}

void main_task(void *args)
{
  hal.init(0, NULL);
  hal.console->printf("AP_HAL SPI Test\r\n\r\n");

  mpu6000_init();

  for (;;) {
    hal.console->printf("\r\nSensor samples at t=%lu:\r\n", xTaskGetTickCount());

    mpu6000_sample_t sample;
    mpu6000_read_sample(&sample);
    mpu6000_print_sample(&sample);

    vTaskDelay(1000);
  }
}

extern "C" int main(void)
{
  xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &htask);

  vTaskStartScheduler();

  for (;;)
    ;

  return 0;
}
