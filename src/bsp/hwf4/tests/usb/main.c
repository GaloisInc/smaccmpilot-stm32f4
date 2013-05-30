/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * main.c --- Character echo test over USB CDC.
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
#include <string.h>

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/gpio.h>
#include <hwf4/led.h>
#include <hwf4/usart.h>
#include <hwf4/usb_cdc.h>

xTaskHandle main_task_handle;

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

/*
 * Main Task
 */

void main_task(void *args)
{
  bool debug_led_state = true;

  led_init();
  usart_init(usart1, 115200);
  usart_enable(usart1);
  usb_cdc_init();

  led_set(0, true);
  debug_puts("\r\nUSB CDC Test\r\n\r\n");

  for (;;) {
    uint8_t ch;

    if (usb_cdc_read_timeout(&ch, 1, 500)) {
      usb_cdc_write(&ch, 1);
      if (ch == '\r') {
        ch = '\n';
        usb_cdc_write(&ch, 1);
      }
    }

    debug_led_state = !debug_led_state;
    led_set(0, debug_led_state);
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
