
#ifndef __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__
#define __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__

#include <stdbool.h>
#include <stdint.h>

struct freertos_semaphore;

void ivory_freertos_semaphore_create(struct freertos_semaphore *);

bool ivory_freertos_semaphore_take(struct freertos_semaphore *,
        uint32_t max_delay);

bool ivory_freertos_semaphore_takenonblocking(struct freertos_semaphore *);

void ivory_freertos_semaphore_give(struct freertos_semaphore *);

#endif // __IVORY_FREERTOS_SEMAPHORE_WRAPPER_H__
