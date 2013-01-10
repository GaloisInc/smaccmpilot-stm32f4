/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * main.c --- PX4 sensor test using SMACCM HW library.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 *
 * Written by James Bielman <jamesjb@galois.com>, 07 December 2012
 */

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>              /* for vsnprintf */
#include <string.h>

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/rcc.h>
#include <hwf4/usb_cdc.h>
#include <hwf4/led.h>
#include <hwf4/spi.h>
#include <hwf4/i2c.h>

/* Make sure we're only built for a PX4 target board. */
#ifndef CONFIG_BOARD_PX4
# error "Unsupported board type."
#endif

xTaskHandle main_task_handle;

/*
 * Utilities
 */

/** Read a 16-bit integer in big endian order from a buffer. */
uint16_t get_be16(const uint8_t *buf)
{
  return ((uint16_t)(buf[0]) << 8) |
         ((uint16_t)(buf[1]) << 0);
}

/** Read a 16-bit integer in little endian order from a buffer. */
uint16_t get_le16(const uint8_t *buf)
{
  return ((uint16_t)(buf[0]) << 0) |
         ((uint16_t)(buf[1]) << 8);
}

/** Read a 24-bit integer in big endian order from a buffer. */
uint32_t get_be24(const uint8_t *buf)
{
  return ((uint32_t)(buf[0]) << 16) |
         ((uint32_t)(buf[1]) << 8) |
         ((uint32_t)(buf[2]) << 0);
}

/*
 * Debug Output
 *
 * Use the USB CDC driver for debug output.
 */

/** Write a string followed by CR/LF to the debug UART. */
static void debug_puts(const char *s)
{
  usb_cdc_write(s, strlen(s));
  usb_cdc_write("\r\n", 2);
}

/** Write formatted output to the debug UART. */
static void debug_printf(const char *fmt, ...)
{
  va_list ap;
  char buf[128];

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  usb_cdc_write(buf, strlen(buf));
  va_end(ap);
}

/*
 * MPU6000
 */

/** SPI device information for the MPU6000. */
struct spi_device mpu6000_dev = {
  pin_b0,
  false,
  SPI_BAUD_DIV_128,
  SPI_CLOCK_POLARITY_LOW,
  SPI_CLOCK_PHASE_1,
  SPI_BIT_ORDER_MSB_FIRST
};

/** Read an 8-bit register from the MPU6000. */
uint8_t mpu6000_read(uint8_t addr)
{
  uint8_t buf[2];

  buf[0] = addr | 0x80;
  buf[1] = 0x00;

  spi_transfer(spi1, &mpu6000_dev, portMAX_DELAY, buf, buf, sizeof(buf));
  return buf[1];
}

/**
 * Read multiple registers from the MPU6000.
 *
 * This reads 'len - 1' registers into 'buf + 1'.  The byte at the
 * beginning is don't care and required for SPI communication.
 */
void mpu6000_read_burst(uint8_t start_addr, uint8_t *buf, size_t len)
{
  buf[0] = start_addr | 0x80;
  spi_transfer(spi1, &mpu6000_dev, portMAX_DELAY, buf, buf, len);
}

/** Write an 8-bit register to the MPU6000. */
void mpu6000_write(uint8_t addr, uint8_t value)
{
  uint8_t buf[2];

  buf[0] = addr;
  buf[1] = value;

  spi_transfer(spi1, &mpu6000_dev, portMAX_DELAY, buf, buf, sizeof(buf));
}

/** Initialize the MPU6000. */
void mpu6000_init(void)
{
  spi_device_init(&mpu6000_dev);
  mpu6000_write(0x6A, 0x10);    /* disable I2C interface */
  mpu6000_write(0x6B, 0x00);    /* wake up, use internal osc */
  mpu6000_write(0x1B, 0x18);    /* gyro at +/- 2000 deg/s */

  uint8_t id = mpu6000_read(0x75);
  debug_printf("MPU6000 Device ID: 0x%02x\r\n", id);
}

/** A raw sample of all sensors on the MPU6000. */
typedef struct {
  int16_t accel_x;
  int16_t accel_y;
  int16_t accel_z;
  int16_t temp;                 /* temp in deg C */
  int16_t gyro_raw_x;
  int16_t gyro_raw_y;
  int16_t gyro_raw_z;
  float   gyro_x;
  float   gyro_y;
  float   gyro_z;
} mpu6000_sample_t;

/** Read a raw sensor sample from the MPU6000. */
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

  sample->gyro_x = (float)sample->gyro_raw_x / 16.4f;
  sample->gyro_y = (float)sample->gyro_raw_y / 16.4f;
  sample->gyro_z = (float)sample->gyro_raw_z / 16.4f;
}

/** Print an MPU6000 sample to the debug console. */
void mpu6000_print_sample(const mpu6000_sample_t *sample)
{
  debug_printf("  mpu6000: acc %d %d %d  temp %d  gyro %.3f %.3f %.3f\r\n",
               sample->accel_x, sample->accel_y, sample->accel_z,
               sample->temp, sample->gyro_x, sample->gyro_y, sample->gyro_z);
}

/*
 * L3GD20 Gyro
 */

/** SPI device information for the L3GD20. */
struct spi_device l3gd20_dev = {
  pin_c14,
  false,
  SPI_BAUD_DIV_128,
  SPI_CLOCK_POLARITY_HIGH,
  SPI_CLOCK_PHASE_2,
  SPI_BIT_ORDER_MSB_FIRST
};

/** Read an 8-bit register from the L3GD20. */
uint8_t l3gd20_read(uint8_t addr)
{
  uint8_t buf[2];

  buf[0] = addr | 0x80;
  buf[1] = 0x00;

  spi_transfer(spi1, &l3gd20_dev, portMAX_DELAY, buf, buf, sizeof(buf));
  return buf[1];
}

/**
 * Read multiple registers from the L3GD20.
 *
 * This reads 'len - 1' registers into 'buf + 1'.  The byte at the
 * beginning is don't care and required for SPI communication.
 */
void l3gd20_read_burst(uint8_t start_addr, uint8_t *buf, size_t len)
{
  buf[0] = start_addr | 0xC0;
  spi_transfer(spi1, &l3gd20_dev, portMAX_DELAY, buf, buf, len);
}

/** Write an 8-bit register to the L3GD20. */
void l3gd20_write(uint8_t addr, uint8_t value)
{
  uint8_t buf[2];

  buf[0] = addr;
  buf[1] = value;

  spi_transfer(spi1, &l3gd20_dev, portMAX_DELAY, buf, buf, sizeof(buf));
}

/** Initialize the L3GD20. */
void l3gd20_init(void)
{
  spi_device_init(&l3gd20_dev);

  /* Normal power mode, XYZ axes enabled, 95Hz ODR. */
  l3gd20_write(0x20, 0x0F);

  /* Full-scale at 2500 dps. */
  l3gd20_write(0x23, 0x30);

  uint8_t id = l3gd20_read(0x0F);
  debug_printf("L3GD20 Device ID:  0x%02x\r\n", id);
}

/** A sample of all sensors on the L3GD20. */
typedef struct {
  int8_t  temp;
  int16_t raw_x;
  int16_t raw_y;
  int16_t raw_z;
  float   x;
  float   y;
  float   z;
} l3gd20_sample_t;

/** Read a raw sensor sample from the L3GD20. */
void l3gd20_read_sample(l3gd20_sample_t *sample)
{
  uint8_t buf[9];

  memset(buf, 0, sizeof(buf));
  l3gd20_read_burst(0x26, buf, sizeof(buf));

  sample->temp   = (int8_t) buf[1];
  /* skip status register data */
  sample->raw_x  = (int16_t) get_le16(&buf[3]);
  sample->raw_y  = (int16_t) get_le16(&buf[5]);
  sample->raw_z  = (int16_t) get_le16(&buf[7]);

  sample->x      = (float) sample->raw_x / 142.857f;
  sample->y      = (float) sample->raw_y / 142.857f;
  sample->z      = (float) sample->raw_z / 142.857f;
}

/** Print a L3GD20 sample to the debug console. */
void l3gd20_print_sample(const l3gd20_sample_t *sample)
{
  debug_printf("  l3gd20:  gyro %.3f %.3f %.3f\r\n",
               sample->x, sample->y, sample->z);
}

/*
 * MS5611 Pressure Sensor
 */

/** I2C address of the MS5611 on the PX4 board. */
#define MS5611_ADDR 0x76

/** Number of PROM registers. */
#define MS5611_NUM_PROM_REGS 8

/** Contents of the MS5611 PROM registers read at init time. */
static uint16_t ms5611_prom[MS5611_NUM_PROM_REGS];

/**
 * Read a 16-bit value from an MS5611 PROM register.
 *
 * @returns true if the read was successful.
 */
bool ms5611_read_prom(uint8_t addr, uint16_t *value_out)
{
  uint8_t buf[2];

  if (i2c_read_reg(i2c2, MS5611_ADDR, 0xA0 | (addr << 1), sizeof(buf), buf)) {
    *value_out = get_be16(buf);
    return true;
  }

  return false;
}

/**
 * Initialize and reset the MS5611.
 *
 * @returns true if the device was initialized successfully.
 */
bool ms5611_init(void)
{
  uint8_t buf[1] = { 0x1E };
  int i;

  if (!i2c_write(i2c2, MS5611_ADDR, buf, sizeof(buf)))
    return false;

  vTaskDelay(10);

  for (i = 0; i < MS5611_NUM_PROM_REGS; ++i) {
    if (!ms5611_read_prom(i, &ms5611_prom[i]))
      return false;
  }

  return true;
}

/** Perform a reading of the pressure value (D1) at OSR=4096. */
bool ms5611_read_pressure(uint32_t *result_out)
{
  uint8_t buf[3];

  buf[0] = 0x48;
  if (!i2c_write(i2c2, MS5611_ADDR, buf, 1))
    return false;

  vTaskDelay(10);

  if (!i2c_read_reg(i2c2, MS5611_ADDR, 0, 3, buf))
    return false;

  *result_out = get_be24(buf);
  return true;
}

/** Perform a reading of the temperature value (D1) at OSR=4096. */
bool ms5611_read_temperature(uint32_t *result_out)
{
  uint8_t buf[3];

  buf[0] = 0x58;
  if (!i2c_write(i2c2, MS5611_ADDR, buf, 1))
    return false;

  vTaskDelay(10);

  if (!i2c_read_reg(i2c2, MS5611_ADDR, 0, 3, buf))
    return false;

  *result_out = get_be24(buf);
  return true;
}

/**
 * A sensor reading from the MS5611.
 *
 * The variable names here are taken from the datasheet.  Also, the
 * intermediate values are available for debugging purposes.
 */
typedef struct {
  uint32_t d1;                  /* digital pressure */
  uint32_t d2;                  /* digital temperature */
  int32_t  dT;                  /* actual/reference temp delta */
  int32_t  temp;                /* in deg C * 100 */
  int64_t  off;                 /* offset at actual temperature */
  int64_t  sens;                /* sensitivity at actual temperature */
  int32_t  pressure;            /* in mbar * 100 */
} ms5611_sample_t;

/**
 * Read and convert a sensor sample from the MS5611.
 *
 * @returns true if the conversion was successful.
 */
bool ms5611_read_sample(ms5611_sample_t *sample)
{
  uint16_t c1 = ms5611_prom[1]; /* prom values with datasheet names */
  uint16_t c2 = ms5611_prom[2];
  uint16_t c3 = ms5611_prom[3];
  uint16_t c4 = ms5611_prom[4];
  uint16_t c5 = ms5611_prom[5];
  uint16_t c6 = ms5611_prom[6];

  if (!ms5611_read_pressure(&sample->d1))
    return false;
  if (!ms5611_read_temperature(&sample->d2))
    return false;

  /* Equations are from the datasheet---careful of overflow. */
  sample->dT   = sample->d2 - ((int32_t)c5 * 256);
  sample->temp = 2000 + (sample->dT * c6 / (1 << 23));
  sample->off  = (int64_t)c2 * 65536 + ((int64_t)c4 * sample->dT) / (1 << 7);
  sample->sens = (int64_t)c1 * (1 << 15) + ((int64_t)c3 * sample->dT) / 256;

  sample->pressure = (sample->d1 * (sample->sens / (1 << 21)) - sample->off) / (1 << 15);

  return true;
}

/** Print an MS5611 sample to the debug UART. */
void ms5611_print_sample(ms5611_sample_t *sample)
{
  debug_printf("  ms5611:  temp %.2f deg C  pressure %.2f mbar\r\n",
               (float)sample->temp / 100.0f,
               (float)sample->pressure / 100.0f);
}

/*
 * Main Task
 */

void main_task(void *args)
{
  led_init();
  usb_cdc_init();
  spi_init(spi1);
  i2c_init(i2c2, pin_b11, pin_b10);

  debug_puts("\r\nPX4 Sensor Test\r\n");
  vTaskDelay(100);
  mpu6000_init();
  l3gd20_init();
  ms5611_init();

  for (;;) {
    debug_printf("\r\nSensor samples at t=%lu:\r\n", xTaskGetTickCount());

    mpu6000_sample_t mpu6000_sample;
    mpu6000_read_sample(&mpu6000_sample);
    mpu6000_print_sample(&mpu6000_sample);

    l3gd20_sample_t l3gd20_sample;
    l3gd20_read_sample(&l3gd20_sample);
    l3gd20_print_sample(&l3gd20_sample);

    ms5611_sample_t ms5611_sample;
    ms5611_read_sample(&ms5611_sample);
    ms5611_print_sample(&ms5611_sample);

    led_set(0, true);
    vTaskDelay(500);
    led_set(0, false);
    vTaskDelay(500);
  }
}

int main(void)
{
  xTaskCreate(main_task, (signed char *) "main", 1024, NULL, 0,
              &main_task_handle);

  vTaskStartScheduler();

  for (;;)
    ;

  return 0;
}
