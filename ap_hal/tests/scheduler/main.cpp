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
#include <hwf4/timer.h>

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle htask, atask;

#define DEBUG_GPIO_1 pin_b10
#define DEBUG_GPIO_2 pin_b11

// Timer procedure to toggle a GPIO pin (I2C2_SCL).
void toggle_pin(uint32_t)
{
  pin_toggle(DEBUG_GPIO_1);
}

// Delay callback that toggles a GPIO pin (I2C2_SDA).
void delay_cb()
{
  pin_toggle(DEBUG_GPIO_2);
}

// Failsafe callback that prints a message.
void failsafe_cb(uint32_t now)
{
  hal.console->printf("missed a timer proc at %lu!\r\n", now);
}

// Task that enters an atomic section for awhile periodically.
void atomic_task(void *args)
{
  for (;;) {
    vTaskDelay(1000);
    hal.scheduler->begin_atomic();
    vTaskDelay(5);
    hal.scheduler->end_atomic();
  }
}

// Expected Output on the Logic analyzer:
// ======================================
//
// - I2C_SCL should be a constant 1kHz square wave.  This tests the
//   timer process.
//
// - I2C_SDA should be a 1kHz square wave for 250ms, then idle for
//   750ms (it might not necessarily be low during this delay).  This
//   demonstrates the delay callback.
//
// - Once per second, the I2C_SCL square wave will be interrupted to
//   test the failsafe callback (by entering an atomic section for
//   longer than 1ms).
void main_task(void *args)
{
  hal.init(0, NULL);            // initializes scheduler
  hal.console->printf("AP_HAL Scheduler Test\r\n");

  pin_enable(DEBUG_GPIO_1);
  pin_reset(DEBUG_GPIO_1);
  pin_set_otype(DEBUG_GPIO_1, PIN_TYPE_PUSHPULL);
  pin_set_ospeed(DEBUG_GPIO_1, PIN_SPEED_100MHZ);
  pin_set_mode(DEBUG_GPIO_1, PIN_MODE_OUTPUT);

  pin_enable(DEBUG_GPIO_2);
  pin_reset(DEBUG_GPIO_2);
  pin_set_otype(DEBUG_GPIO_2, PIN_TYPE_PUSHPULL);
  pin_set_ospeed(DEBUG_GPIO_2, PIN_SPEED_100MHZ);
  pin_set_mode(DEBUG_GPIO_2, PIN_MODE_OUTPUT);

  hal.scheduler->register_timer_failsafe(failsafe_cb, 1000);
  hal.scheduler->register_timer_process(toggle_pin);
  hal.scheduler->register_delay_callback(delay_cb, 0);

  for (;;) {
    portTickType now = xTaskGetTickCount();
    hal.console->printf("tick at t=%lu!\r\n", now);

    hal.scheduler->delay(250);
    vTaskDelay(750);
  }
}

extern "C" int main(void)
{
  xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &htask);
  xTaskCreate(atomic_task, (signed char *)"main", 128, NULL, 0, &atask);

  vTaskStartScheduler();

  for (;;)
    ;

  return 0;
}
