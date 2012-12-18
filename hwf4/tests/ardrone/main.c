/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * main.c --- AR.Drone motor test.
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
#include <stdio.h>
#include <string.h>

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/gpio.h>
#include <hwf4/led.h>
#include <hwf4/usart.h>
#include <hwf4/timer.h>
#include <hwf4/ardrone.h>

xTaskHandle main_task_handle;

/*
 * Debug Output
 *
 * Use the USB CDC driver for debug output.
 */

/** Write a string followed by CR/LF to the debug UART. */
static void debug_puts(const char *s)
{
  usart_write(usart1, (uint8_t *)s, strlen(s));
  usart_write(usart1, (uint8_t *)"\r\n", 2);
}

/** Write formatted output to the debug UART. */
void debug_printf(const char *fmt, ...)
{
  va_list ap;
  char buf[128];

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  usart_write(usart1, (uint8_t *)buf, strlen(buf));
  va_end(ap);
}

/*
 * Main Task
 */

void main_task(void *args)
{
  portTickType ticks, now;
  int motor = 1;

  led_init();
  timer_init();
  usart_init(usart1, 115200);
  usart_enable(usart1);
  usart_init(usart2, 115200);
  usart_enable(usart2);

  led_set(0, true);
  debug_puts("\r\nAR.Drone Motor Test\r\n");

  ardrone_init();
  ardrone_motor_init();

  ticks = xTaskGetTickCount();
  debug_printf("using motor %d\r\n", motor);

  for (;;) {
    ardrone_motor_set(
      motor == 1 ? 10 : 0,
      motor == 2 ? 10 : 0,
      motor == 3 ? 10 : 0,
      motor == 4 ? 10 : 0);

    now = xTaskGetTickCount();
    if (now - ticks > 2000) {
      motor = (motor == 4 ? 1 : motor + 1);
      debug_printf("using motor %d\r\n", motor);
      ticks = now;
    }

    vTaskDelay(5);
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
