#include <FreeRTOS.h>
#include <queue.h>

#include "tower.h"

/* extern xQueueHandle update_queue; */
extern xQueueHandle eventHeapchannel_0_endpoint_verify_updates_1;

/* struct { */
/*     int id; */
/*     void *value; */
/* } new_value_s; */

struct assignment new_value_s;

/* Defined in legacy.c and set to the tower-generated send queue function. */
extern void (*record_send)(const struct assignment *);

/* This is our FreeRTOS-specific implementation of what to do when called by the
   gcc plugin instrumentation. */
void record_assignment(int id, void *new_value)
/* void record_assignment(int id, void *new_value) */
{
  /* if (update_queue) { */
  /*   new_value_s.id = id; */
  /*   new_value_s.value = new_value; */
  /*   xQueueSendToBack(update_queue, &new_value_s, portMAX_DELAY); */
  /* } */

  new_value_s.var_id = id;
  new_value_s.value = (uint32_t) new_value;
  record_send(&new_value_s);

}
