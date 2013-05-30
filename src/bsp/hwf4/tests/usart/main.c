/*
 * main.c
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/usart.h>

#if defined(CONFIG_BOARD_STM32F4_DISCOVERY)
# define DEBUG_USART usart1
#elif defined(CONFIG_BOARD_PX4)
# define DEBUG_USART usart1
#else
# error "Unsupported board type."
#endif

xTaskHandle main_task_handle;

void main_task(void *args) {
    uint8_t buf = 0;

    usart_write(DEBUG_USART, (uint8_t *)"Enter some text:\r\n", 18);

    for(;;) {
        usart_read(DEBUG_USART, &buf, 1);
        usart_write(DEBUG_USART, &buf, 1);

        if (buf == '\r') {
            buf = '\n';
            usart_write(DEBUG_USART, &buf, 1);
        }
    }
}

int main(void) {

    xTaskCreate(main_task, (signed char *)"main_task", 1000, NULL, 0,
                &main_task_handle);

    usart_init(DEBUG_USART, 115200);
    usart_enable(DEBUG_USART);

    vTaskStartScheduler();

    for(;;);

    return 0;
}
