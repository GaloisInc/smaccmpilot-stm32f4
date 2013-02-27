/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * main.c --- Timer test program.
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
#include <hwf4/usart.h>
#include <hwf4/timer.h>

/* Make sure we're only built for a PX4 target board. */
#ifndef CONFIG_BOARD_PX4
# error "Unsupported board type."
#endif

xTaskHandle main_task_handle;

/*
 * Debug UART
 *
 * We use USART1 for debug output as it is connected to the FTDI
 * connector on the PX4 IO board.
 */

#define DEBUG_UART usart1

/** Initialize the debug UART. */
void debug_uart_init(void)
{
  usart_init(DEBUG_UART, 115200);
  usart_enable(DEBUG_UART);
}

/** Write a string followed by CR/LF to the debug UART. */
void debug_puts(const char *s)
{
  usart_write(DEBUG_UART, (uint8_t *)s, strlen(s));
  usart_write(DEBUG_UART, (uint8_t *)"\r\n", 2);
}

/** Write formatted output to the debug UART. */
void debug_printf(const char *fmt, ...)
{
  va_list ap;
  char buf[128];

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  usart_write(DEBUG_UART, (uint8_t *)buf, strlen(buf));
  va_end(ap);
}

/*
 * Main Task
 */

void main_task(void *args)
{
  debug_uart_init();

  debug_puts("\r\nTimer Test\r\n");
  timer_init();

  for (;;) {
    timer_tick_t ticks = timer_get_ticks();
    debug_printf("time: %llu usec (%.3f sec)\r\n",
                 ticks, (double)ticks / 1000000.0);

    uint16_t ppm[PPM_MAX_CHANNELS];
    size_t len = timer_get_ppm(ppm, PPM_MAX_CHANNELS, NULL);

    debug_printf("ppm:  ");
    for (int i = 0; i < len; ++i)
      debug_printf("%u ", ppm[i]);
    debug_printf("\r\n\r\n");

    vTaskDelay(1000);
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
