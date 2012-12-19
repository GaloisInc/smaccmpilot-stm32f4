/*
 * main.cpp --- AP_HAL console driver test.
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

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle htask;

void main_task(void *args)
{
  hal.init(0, NULL);
  hal.console->printf("AP_HAL Console Test\r\n");

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
