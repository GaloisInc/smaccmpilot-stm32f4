/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * main.c --- Timer usleep test program.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 *
 * Written by James Bielman <jamesjb@galois.com>, 07 December 2012
 */

#include <stdbool.h>
#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/gpio.h>
#include <hwf4/timer.h>

/* Make sure we're only built for a PX4 target board. */
#ifndef CONFIG_BOARD_PX4
# error "Unsupported board type."
#endif

/* We start two tasks which twiddle the I2C2_SDA and I2C2_SCK GPIOs at
 * different rates, using "timer_usleep" in between edges.
 *
 * What we end up seeing is each task twiddle its GPIO during its
 * timeslice while the other line is frozen, then when a context
 * switch occurs, they swap. */
xTaskHandle task1_handle;
xTaskHandle task2_handle;

/** Information passed as a parameter to each task. */
struct task_data {
  uint16_t us_delay;
  struct pin *pin;
  bool pin_state;
};

struct task_data task1_data = { 20, pin_b10, false };
struct task_data task2_data = { 50, pin_b11, false };

/*
 * Main Task
 */

void main_task(void *arg)
{
  struct task_data *data = (struct task_data *)arg;

  pin_enable(data->pin);
  pin_reset(data->pin);
  pin_set_otype(data->pin, PIN_TYPE_PUSHPULL);
  pin_set_ospeed(data->pin, PIN_SPEED_100MHZ);
  pin_set_mode(data->pin, PIN_MODE_OUTPUT);

  for (;;) {
    timer_usleep(data->us_delay);
    data->pin_state = !data->pin_state;

    if (data->pin_state)
      pin_set(data->pin);
    else
      pin_reset(data->pin);
  }
}

int main(void)
{
  timer_init();

  xTaskCreate(main_task, (signed char *)"task1", 256, &task1_data, 0, &task1_handle);
  xTaskCreate(main_task, (signed char *)"task2", 256, &task2_data, 0, &task2_handle);

  vTaskStartScheduler();

  for (;;)
    ;

  return 0;
}
