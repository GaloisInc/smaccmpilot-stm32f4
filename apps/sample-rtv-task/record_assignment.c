#include <FreeRTOS.h>
#include <queue.h>

#include "tower-hdrs/queueStruct.h"

/* extern xQueueHandle update_queue; */
extern xQueueHandle eventHeapchannel_0_endpoint_verify_updates_1;

/* struct { */
/*     int id; */
/*     void *value; */
/* } new_value_s; */

struct assignment new_value_s;

/* This is our FreeRTOS-specific implementation of what to do when called by the
   gcc plugin instrumentation. */
void record_assignement(int id, void *new_value)
/* void record_assignment(int id, void *new_value) */
{
    /* if (update_queue) { */
    /*     new_value_s.id = id; */
    /*     new_value_s.value = new_value; */
    /*     xQueueSendToBack(update_queue, &new_value_s, portMAX_DELAY); */
    /* } */

    if (eventHeapchannel_0_endpoint_verify_updates_1) {
        new_value_s.var_id = id;
        new_value_s.value = new_value;
        xQueueSendToBack(eventHeapchannel_0_endpoint_verify_updates_1, &new_value_s, portMAX_DELAY);
    }
}
