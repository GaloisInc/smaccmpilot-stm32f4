/*
 * main.cpp --- AP_HAL sensors test.
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

#include <hwf4/led.h>

#if CONFIG_HAL_BOARD == HAL_BOARD_SMACCM
# include <AP_HAL_SMACCM.h>
#else
# error "Unsupported CONFIG_HAL_BOARD type."
#endif
#include <AP_Param.h>
#include <AP_InertialSensor_MPU6000.h>
#include <AP_Compass_HMC5843.h>
#include <AP_Baro.h>

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// The rate we run the inertial sensor at.
static const AP_InertialSensor::Sample_rate INS_SAMPLE_RATE =
  AP_InertialSensor::RATE_200HZ;

static AP_InertialSensor_MPU6000 g_ins;
static AP_Compass_HMC5843 g_compass;
static AP_Baro_MS5611 g_baro(&AP_Baro_MS5611::i2c);

static xTaskHandle htask;

void flash_leds(bool on)
{
  led_set(0, on);
}

// Since we don't have the data interrupt, we poll the inertial sensor
// from a timer process.
void sample_mpu6000(uint32_t)
{
  AP_InertialSensor_MPU6000::data_interrupt();
}

void sample_compass(uint32_t)
{
  g_compass.read();
  g_compass.accumulate();
}

void main_task(void *args)
{
  led_init();

  hal.init(0, NULL);
  hal.console->printf("AP_HAL Sensor Test\r\n");

  hal.console->printf("init AP_Param: ");
  AP_Param::setup_sketch_defaults();
  hal.console->printf("done\r\n");

  hal.console->printf("init AP_Compass: ");
  g_compass.init();

  g_compass.set_orientation(AP_COMPASS_COMPONENTS_DOWN_PINS_BACK);
  g_compass.set_offsets(0,0,0);
  g_compass.set_declination(ToRad(0.0));
  hal.scheduler->register_timer_process(sample_compass);
  hal.console->printf("done\r\n");

  hal.console->printf("init AP_Baro: ");
  g_baro.init();
  g_baro.calibrate();
  hal.console->printf("done\r\n");

  hal.console->printf("init AP_InertialSensor: ");
  hal.scheduler->register_timer_process(sample_mpu6000);
  g_ins.init(AP_InertialSensor::COLD_START, INS_SAMPLE_RATE, flash_leds);
  g_ins.init_accel(flash_leds);
  hal.console->println();
  led_set(0, false);
  hal.console->printf("done\r\n");

  for (;;) {
    vTaskDelay(100);
    g_ins.update();

    hal.console->printf("\r\n");

    // It'd be nice to print the temperature here too, but it's
    // stubbed out in the driver to always return 20.0 degC.
    Vector3f accel(g_ins.get_accel());
    Vector3f gyro(g_ins.get_gyro());
    hal.console->printf("mpu6000: accel %.2f %.2f %.2f  "
                                 "gyro %.2f %.2f %.2f\r\n",
                        accel.x, accel.y, accel.z,
                        gyro.x, gyro.y, gyro.z);

    hal.console->printf("compass: heading %.2f deg\r\n",
                        ToDeg(g_compass.calculate_heading(0, 0)));
    g_compass.null_offsets();

    g_baro.read();
    if (!g_baro.healthy) {
      hal.console->printf("baro:    not healthy\r\n");
    } else {
      hal.console->printf("baro:    pres %.2f  temp %.2f  alt %.3f  climb %.2f\r\n",
                          g_baro.get_pressure(), g_baro.get_temperature(),
                          g_baro.get_altitude(), g_baro.get_climb_rate());
    }
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
