#include <FreeRTOS.h>
#include <queue.h>

#include "tower.h"

struct assignment new_value_s;

/* Defined in legacy.c and set to the tower-generated send queue function. */
extern void (*record_send)(const struct assignment *);

/* This is our FreeRTOS-specific implementation of what to do when called by the
   gcc plugin instrumentation. */
void record_assignment(int id, void *new_value) {
  new_value_s.var_id = id;
  new_value_s.value = (uint32_t) new_value;
  record_send(&new_value_s);
}
