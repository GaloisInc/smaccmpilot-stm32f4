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

#include <hwf4/i2c.h>
#include <hwf4/rcc.h>
#include <hwf4/gpio.h>

xTaskHandle main_task_handle;

int successes, failures = 0;
int chipidreg = 0;

void main_task(void *args) {
    
    /* On stm32f4 discovery board: 
     * PA4 is GPIO     <-> CS43L22 LRCK/AIN1x
     * PB9 is I2C1_SDA <-> CS43L22 SDA
     * PB6 is I2C1_SCL <-> CS43L22 SCL
     * PC7 is GPIO     <-> CS43L22 MCLK
     * PD4 is GPIO     <-> CS43L22 RESET (must pull high to power on)
     */

    /* Pull the CS43L22 out of reset */
    pin_enable(pin_d4);
    pin_set_mode(pin_d4, PIN_MODE_OUTPUT);
    pin_set_otype(pin_d4, PIN_TYPE_PUSHPULL);
    pin_set(pin_d4);

    i2c_init(i2c1, pin_b9, pin_b6);

    for(;;) {
        uint8_t result;
        /* Register 1 should be the Chip ID and Revision
         * top 5 bits are Chip ID : should be 11100 
         * bottom 3 bits are revision: should be in range 0..3 */
        bool error = i2c_read_reg(i2c1, 0x4a, 1, 1, &result);
        if (!error) {
            chipidreg = result;
            successes++;
        } else {
            failures++;
        }
        vTaskDelay(1000);
    }
}

int main(void) {

    xTaskCreate(main_task, (signed char *)"main_task", 1000, NULL, 0,
            &main_task_handle);


    vTaskStartScheduler();

    for(;;);

    return 0;
}
