/*******************************************************************************
 * Example task that uses check_properties().
 *
 * This task listens for updates on a FreeRTOS queue and when it receives an
 * update it does the following:
 *
 *   1) Appends the new value to the history -- passing the variable id along
 *   2) Calls check_properties
 *
 * If the check_properties call returns false, it lights a LED.
 * The xQueueHandle defined in this file needs to be used by the task sending
 * the update evente (i.e. the record_assignment function).
 *
 * To compile for use with FreeRTOS on an stm32f4 (PX4):
 *
 * CC=arm-none-eabi-gcc
 * CFLAGS=-std=c99 -mlittle-endian -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16
 * FREERTOS_DIR=../../OutsideSrcs/FreeRTOS/FreeRTOS/
 * INCLUDES=-I$(OUTDIR) \
 *     -I$(FREERTOS_DIR)/Source/include \
 *     -I$(FREERTOS_DIR)/Source/portable/GCC/ARM_CM4F \
 *     -I../../source/stm32f4/bsp/include \
 *     -I. -I../../source/dsl/ivory-lang-c/runtime/
 *
 * all:
 *     $(CC) $(CFLAGS) $(INCLUDES) -c checker_task.c
 ******************************************************************************/

#include <stdbool.h>
#include <FreeRTOS.h>
#include <queue.h>
#include "generated/instrumented.h"

bool check_properties();
bool led_set(int led, bool state);

struct assignment {
    int var_id;
    void *value;
} new_value_s;

xQueueHandle update_queue;

void verify_updates(void *args)
{
    for (;;) {
        xQueueReceive(update_queue, &new_value_s, portMAX_DELAY);

        append_to_history(new_value_s.var_id, new_value_s.value);

        if (!check_properties()) {
            led_set(1, 1);
        }
    }
}
