/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * main.c --- EEPROM driver test.
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

#include <hwf4/led.h>
#include <hwf4/usb_cdc.h>
#include <hwf4/i2c.h>
#include <hwf4/eeprom.h>

/* Make sure we're only built for a PX4 target board. */
#ifndef CONFIG_BOARD_PX4
# error "Unsupported board type."
#endif

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

/** Write formatted output to the debug UART. */
static void debug_printf(const char *fmt, ...)
{
  va_list ap;
  char buf[128];

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  usb_cdc_write(buf, strlen(buf));
  va_end(ap);
}

/** Hex dump a buffer to the debug UART. */
static void hexdump(const void *p, size_t len)
{
  const uint8_t *buf = (const uint8_t *)p;
  size_t addr;

  for (addr = 0; addr < len; ++addr) {
    if ((addr % 16) == 0) {
      if (addr != 0)
        debug_printf("\r\n");
      debug_printf("%04x ", addr);
    } else if ((addr % 8) == 0 && addr != 0) {
      debug_printf(" ");
    }

    debug_printf("%02x ", buf[addr]);
  }

  debug_printf("\r\n");
}

/*
 * Main Task
 */

/** Number of bytes of test data to write. */
#define TEST_DATA_LEN 317

/** Offset within EEPROM to write test data to. */
#define TEST_DATA_OFFSET 31

void main_task(void *args)
{
  uint16_t addr;
  uint8_t buf[TEST_DATA_LEN];
  bool led_state = false;

  led_init();
  usb_cdc_init();
  i2c_init(i2c2, pin_b11, pin_b10);
  eeprom_init(i2c2, 0x50);

  /* Give the user some time to attach the USB console. */
  vTaskDelay(5000);

  led_set(0, false);
  debug_puts("\r\nEEPROM Test\r\n\r\n");

  /* Write some test data to the EEPROM. */
  for (addr = 0; addr < sizeof(buf); ++addr)
    buf[addr] = addr & 0xFF;

  if (eeprom_write(TEST_DATA_OFFSET, buf, sizeof(buf)) < 0)
    debug_puts("eeprom block write failed");

  /* Read the test data in one shot and print it out. */
  memset(buf, 0, sizeof(buf));

  if (eeprom_read(TEST_DATA_OFFSET, buf, sizeof(buf)) < 0) {
    debug_puts("eeprom block read failed");
  } else {
    debug_puts("Test Data:");
    hexdump(buf, sizeof(buf));
  }

  for (;;) {
    led_state = !led_state;
    led_set(0, led_state);

    vTaskDelay(500);
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
