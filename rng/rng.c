
#include <stdint.h>
#include <stdbool.h>

#include <FreeRTOS.h>
#include <queue.h>
#include <task.h>

#include <stm32f4xx.h>
#include <misc.h>
#include <stm32f4xx_rcc.h>
#include <stm32f4xx_rng.h>


#define BUFFER_SIZE 16


static xQueueHandle rng_buffer = {0};

void rng_init(void) {

    // enable the clock
    RCC_AHB2PeriphClockCmd(RCC_AHB2Periph_RNG, ENABLE);

    // configure the interrupt
    NVIC_InitTypeDef nvic_cfg                  = {0};
    nvic_cfg.NVIC_IRQChannel                   = HASH_RNG_IRQn;
    nvic_cfg.NVIC_IRQChannelPreemptionPriority = 0;
    nvic_cfg.NVIC_IRQChannelSubPriority        = 0;
    nvic_cfg.NVIC_IRQChannelCmd                = ENABLE;
    NVIC_Init(&nvic_cfg);

    // configure the RNG
    RNG_Cmd(ENABLE);

    rng_buffer = xQueueCreate(BUFFER_SIZE, sizeof(uint32_t));
}

bool rng_word(portTickType timeout, uint32_t *result) {
    // try to receive some data right away
    if(pdTRUE != xQueueReceive(rng_buffer, result, (portTickType)0)) {
        RNG_ITConfig(ENABLE);

        return pdTRUE == xQueueReceive(rng_buffer, result, timeout);
    }

    return true;
}

void rng_irq_handler(void) {
    portBASE_TYPE shouldYield = pdFALSE;
    uint32_t word = 0;

    if(RNG_GetFlagStatus(RNG_FLAG_DRDY) == SET) {
        word = RNG_GetRandomNumber();
        if(pdTRUE != xQueueSendFromISR(rng_buffer, &word, &shouldYield)) {
                RNG_ITConfig(DISABLE);
        }
    }

    if(pdTRUE == shouldYield) {
        taskYIELD();
    }
}
