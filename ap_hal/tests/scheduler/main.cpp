/*
 * main.cpp --- AP_HAL scheduler test.
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

#include <hwf4/gpio.h>

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle htask;

#define DEBUG_GPIO pin_b10

// Timer procedure to toggle a GPIO pin (I2C2_SCL).
void toggle_pin(uint32_t)
{
  pin_toggle(DEBUG_GPIO);
}

void main_task(void *args)
{
  hal.init(0, NULL);            // initializes scheduler
  hal.console->printf("AP_HAL Scheduler Test\r\n");

  pin_enable(DEBUG_GPIO);
  pin_reset(DEBUG_GPIO);
  pin_set_otype(DEBUG_GPIO, PIN_TYPE_PUSHPULL);
  pin_set_ospeed(DEBUG_GPIO, PIN_SPEED_100MHZ);
  pin_set_mode(DEBUG_GPIO, PIN_MODE_OUTPUT);

  hal.scheduler->register_timer_process(toggle_pin);

  for (;;) {
    portTickType now = xTaskGetTickCount();
    hal.console->printf("tick at t=%lu!\r\n", now);
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
