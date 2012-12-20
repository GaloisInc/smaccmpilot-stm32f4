/*
 * main.cpp --- AP_HAL I2C driver test.
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

/** Read a 16-bit integer in big endian order from a buffer. */
uint16_t get_be16(const uint8_t *buf)
{
  return ((uint16_t)(buf[0]) << 8) |
         ((uint16_t)(buf[1]) << 0);
}

/** Read a 24-bit integer in big endian order from a buffer. */
uint32_t get_be24(const uint8_t *buf)
{
  return ((uint32_t)(buf[0]) << 16) |
         ((uint32_t)(buf[1]) << 8) |
         ((uint32_t)(buf[2]) << 0);
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

  if (hal.i2c->readRegisters(MS5611_ADDR, 0xA0 | (addr << 1),
                             sizeof(buf), buf) == 0) {
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

  if (hal.i2c->write(MS5611_ADDR, sizeof(buf), buf) != 0)
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
  if (hal.i2c->write(MS5611_ADDR, 1, buf) != 0)
    return false;

  vTaskDelay(10);

  if (hal.i2c->readRegisters(MS5611_ADDR, 0, 3, buf) != 0)
    return false;

  *result_out = get_be24(buf);
  return true;
}

/** Perform a reading of the temperature value (D1) at OSR=4096. */
bool ms5611_read_temperature(uint32_t *result_out)
{
  uint8_t buf[3];

  buf[0] = 0x58;
  if (hal.i2c->write(MS5611_ADDR, 1, buf) != 0)
    return false;

  vTaskDelay(10);

  if (hal.i2c->readRegisters(MS5611_ADDR, 0, 3, buf))
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
  hal.console->printf("  ms5611:  temp %.2f deg C  pressure %.2f mbar\r\n",
                      (float)sample->temp / 100.0f,
                      (float)sample->pressure / 100.0f);
}

void main_task(void *args)
{
  hal.init(0, NULL);
  hal.uartA->begin(115200);
  hal.console->init(hal.uartA);
  hal.console->printf("AP_HAL I2C Test\r\n");
  hal.i2c->begin();

  ms5611_init();

  for (;;) {
    hal.console->printf("\r\nSensor samples at t=%lu:\r\n", xTaskGetTickCount());

    ms5611_sample_t sample;
    if (!ms5611_read_sample(&sample))
      hal.console->print("ms5611: reading sample failed\r\n");
    else
      ms5611_print_sample(&sample);

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
