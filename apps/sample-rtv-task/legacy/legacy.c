#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>
#include <stdint.h>

#include <hwf4/led.h>

#include "record_assignment.h"

#include "tower.h"

xTaskHandle update_time_task_handle;
xTaskHandle read_clock_task_handle;
xTaskHandle checker_task_handle;

/* xQueueHandle update_queue; */
/* void verify_updates(void *args); */

/* Global variable that keeps track of time. */
int32_t curr_time __attribute__((instrument(0)));

int32_t dummy __attribute__((instrument(1)));

/* Task simulating reading from a clock every second and putting the result in
     * the given queue */

/* uh-oh! this will overflow after 10 seconds */
int32_t fake_clock_time = 0x7ffffff6;

void read_clock_block(void (*send)(const int32_t *))
{
    send(&fake_clock_time);
    fake_clock_time++;
  }

/* Set it to the tower-generated send function. */
void (*record_send)(const struct assignment *);

void update_time_init(void (*check_send)(const struct assignment *)) {

  record_send = check_send;
  led_init();
  led_set(0, 0);
  led_set(1, 0);
  }

/* Task that updates the global time variable when a new time reading is
     * received */
void update_time_block(int32_t new_time)
{
  /* XXX just so the dummy var has some assignment to call the plugin. */
  dummy = 100;

  /* This assignment is what gets instrumented with a call to
           record_assignment() */
  curr_time = new_time;
  led_set(0, curr_time % 2);
 }

int main(void) {
    tower_entry();
    vTaskStartScheduler();
    for(;;);

    return 0;
 }

/* int main(void) */
/* { */
/*     /\* The communication channel between the two tasks is simply a one-element */
/*        queue *\/ */
/*     xQueueHandle update_time_queue = xQueueCreate(1, sizeof(int)); */
/*     /\* update_queue = xQueueCreate(128, sizeof(int32_t)); *\/ */

/*     tower_entry(); */

/*     /\* The checker task.  Defined in checker_task.c *\/ */
/*     /\* xTaskCreate(verify_updates, (signed char *)"verify_updates", 1000, update_queue, 100, &checker_task_handle); *\/ */

/*     xTaskCreate(read_clock_task, (signed char *)"read_clock_task", 1000, update_time_queue, 1, &read_clock_task_handle); */

/*     xTaskCreate(update_time_task, (signed char *)"update_time_task", 1000, update_time_queue, 0, &update_time_task_handle); */

/*     vTaskStartScheduler(); */
/*     for(;;); */

/*     return 0; */
/* } */
