#include <FreeRTOS.h>
#include <queue.h>

extern xQueueHandle update_queue;

struct {
    int id;
    void *value;
} new_value_s;

void record_assignment(int id, void *new_value)
{
    if (update_queue) {
        new_value_s.id = id;
        new_value_s.value = new_value;
        xQueueSendToBack(update_queue, &new_value_s, portMAX_DELAY);
    }
}
