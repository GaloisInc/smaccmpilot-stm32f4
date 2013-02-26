
#include "freertos_semaphore_wrapper.h"

#include <FreeRTOS.h>
#include <semphr.h>

struct freertos_semaphore {
    xSemaphoreHandle sem;
};

void ivory_freertos_semaphore_create(struct freertos_semaphore *wrap)
{
    wrap->sem = xSemaphoreCreateMutex();
}

bool ivory_freertos_semaphore_take(struct freertos_semaphore *wrap,
        uint32_t max_delay)
{
    if (xSemaphoreTake(wrap->sem, max_delay) == pdTRUE)
        return true;
    else
        return false;
}

bool ivory_freertos_semaphore_takenonblocking(struct freertos_semaphore *wrap)
{
    return ivory_freertos_semaphore_take(wrap,0);
}

void ivory_freertos_semaphore_give(struct freertos_semaphore *wrap)
{
    xSemaphoreGive(wrap->sem);
}


