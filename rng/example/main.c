
#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>

#include <rng.h>

static xTaskHandle main_task_handle = {0};

void HASH_RNG_IRQHandler(void) {
    rng_irq_handler();
}

void main_task(void *_params) {
    uint32_t word = 0;

    for(;;) {
        rng_word(portMAX_DELAY, &word);

        vTaskDelay(1000);
    }
}

int main(void) {
    xTaskCreate(main_task, (signed portCHAR *)"main_task", 1000, NULL, 0,
            &main_task_handle);

    rng_init();

    vTaskStartScheduler();

    for(;;);
}
