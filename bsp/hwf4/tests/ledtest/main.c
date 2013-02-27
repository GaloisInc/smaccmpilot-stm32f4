/*
 * main.c
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

xTaskHandle main_task_handle;

void main_task(void *args)
{
  int i;

  led_init();

  for(;;) {
    for (i = 0; i < led_count(); ++i)
      led_set(i, (i & 1) == 0);
    vTaskDelay(1000);

    for (i = 0; i < led_count(); ++i)
      led_set(i, (i & 1) != 0);
    vTaskDelay(1000);
  }
}

int main(void)
{
  xTaskCreate(main_task, (signed char *)"main_task", 1000, NULL, 0,
              &main_task_handle);

  vTaskStartScheduler();

  for(;;)
    ;

  return 0;
}
