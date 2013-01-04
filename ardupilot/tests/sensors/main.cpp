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
#include <hwf4/gpio.h>

#if CONFIG_HAL_BOARD == HAL_BOARD_SMACCM
# include <AP_HAL_SMACCM.h>
#else
# error "Unsupported CONFIG_HAL_BOARD type."
#endif
#include <AP_AHRS.h>
#include <AP_Baro.h>
#include <AP_Compass_HMC5843.h>
#include <AP_InertialSensor_MPU6000.h>
#include <AP_Param.h>

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// The rate we run the inertial sensor at.
static const AP_InertialSensor::Sample_rate INS_SAMPLE_RATE =
  AP_InertialSensor::RATE_200HZ;

static AP_InertialSensor_MPU6000 g_ins;
static AP_Compass_HMC5843 g_compass;
static AP_Baro_MS5611 g_baro(&AP_Baro_MS5611::i2c);
static GPS *g_gps;
static AP_AHRS_DCM g_ahrs(&g_ins, g_gps);

static xTaskHandle htask;

void flash_leds(bool on)
{
  led_set(0, on);
}

void failsafe(uint32_t)
{
  hal.console->write('?');
}

void main_task(void *args)
{
  vTaskSetApplicationTaskTag(xTaskGetIdleTaskHandle(), (pdTASK_HOOK_CODE)1);
  vTaskSetApplicationTaskTag(NULL, (pdTASK_HOOK_CODE)2);

  led_init();

  hal.init(0, NULL);
  hal.console->printf("AP_HAL Sensor Test\r\n");
  hal.scheduler->register_timer_failsafe(failsafe, 1000);

  hal.console->printf("init AP_InertialSensor: ");
  g_ins.init(AP_InertialSensor::COLD_START, INS_SAMPLE_RATE, flash_leds);
  g_ins.init_accel(flash_leds);
  hal.console->println();
  led_set(0, false);
  hal.console->printf("done\r\n");

  hal.console->printf("init AP_Compass: ");
  g_compass.init();
  g_compass.set_orientation(AP_COMPASS_COMPONENTS_DOWN_PINS_BACK);
  g_compass.set_offsets(0,0,0);
  g_compass.set_declination(ToRad(0.0));
  hal.console->printf("done\r\n");

  hal.console->printf("init AP_Baro: ");
  g_baro.init();
  g_baro.calibrate();
  hal.console->printf("done\r\n");

  hal.console->printf("init AP_AHRS: ");
  g_ahrs.init();
  g_ahrs.set_compass(&g_compass);
  g_ahrs.set_barometer(&g_baro);
  hal.console->printf("done\r\n");

  portTickType last_print = 0;
  portTickType last_compass = 0;
  portTickType last_wake = 0;
  float heading = 0.0f;

  last_wake = xTaskGetTickCount();

  for (;;) {
    // Delay to run this loop at 100Hz.
    vTaskDelayUntil(&last_wake, 10);

    portTickType now = xTaskGetTickCount();

    if (last_compass == 0 || now - last_compass > 100) {
      last_compass = now;
      g_compass.read();
      g_baro.read();
      heading = g_compass.calculate_heading(g_ahrs.get_dcm_matrix());
    }

    g_ahrs.update();

    if (last_print == 0 || now - last_print > 100) {
      last_print = now;

      hal.console->write("\r\n");
      hal.console->printf("ahrs:    roll %4.1f  pitch %4.1f  yaw %4.1f hdg %.1f\r\n",
                          ToDeg(g_ahrs.roll), ToDeg(g_ahrs.pitch),
                          ToDeg(g_ahrs.yaw),
                          g_compass.use_for_yaw() ? ToDeg(heading) : 0.0f);

      Vector3f accel(g_ins.get_accel());
      Vector3f gyro(g_ins.get_gyro());
      hal.console->printf("mpu6000: accel %.2f %.2f %.2f  "
                          "gyro %.2f %.2f %.2f\r\n",
                          accel.x, accel.y, accel.z,
                          gyro.x, gyro.y, gyro.z);

      hal.console->printf("compass: heading %.2f deg\r\n",
                          ToDeg(g_compass.calculate_heading(0, 0)));
      g_compass.null_offsets();
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
