/*
 * main.cpp --- AP_InertialSensor_MPU6000 test.
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
#include <AP_InertialSensor_MPU6000.h>

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// The rate we run the inertial sensor at.
static const AP_InertialSensor::Sample_rate SAMPLE_RATE = AP_InertialSensor::RATE_200HZ;

static AP_InertialSensor_MPU6000 g_ins;

static xTaskHandle htask;

void flash_leds(bool on)
{
  led_set(0, on);
}

// Since we don't have the data interrupt, we poll the inertial sensor
// from a timer process.
void sample_sensor(uint32_t)
{
  AP_InertialSensor_MPU6000::data_interrupt();
}

void main_task(void *args)
{
  led_init();

  hal.init(0, NULL);
  hal.console->printf("AP_InertialSensor_MPU6000 Test\r\n");

  hal.scheduler->register_timer_process(sample_sensor);
  g_ins.init(AP_InertialSensor::COLD_START, SAMPLE_RATE, flash_leds);
  g_ins.init_accel(flash_leds);
  hal.console->println();
  led_set(0, false);

  for (;;) {
    vTaskDelay(100);
    g_ins.update();

    Vector3f accel(g_ins.get_accel());
    Vector3f gyro(g_ins.get_gyro());

    // It'd be nice to print the temperature here too, but it's
    // stubbed out in the driver to always return 20.0 degC.
    hal.console->printf("mpu6000: accel %.2f %.2f %.2f  "
                                 "gyro %.2f %.2f %.2f\r\n",
                        accel.x, accel.y, accel.z,
                        gyro.x, gyro.y, gyro.z);
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
