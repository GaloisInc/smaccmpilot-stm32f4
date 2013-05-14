#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>

#include <hwf4/led.h>

#include "tower-hdrs/tower.h"

xTaskHandle update_time_task_handle;
xTaskHandle read_clock_task_handle;
xTaskHandle checker_task_handle;

/* xQueueHandle update_queue; */
/* void verify_updates(void *args); */

/* Global variable that keeps track of time. */
int32_t curr_time __attribute__((instrument(0)));

int32_t dummy __attribute__((instrument(1))) = 100;

/* Task simulating reading from a clock every second and putting the result in
 * the given queue */
void read_clock_task(void *args)
{
    xQueueHandle update_time_queue = (xQueueHandle)args;
    int fake_clock_time = 0x7ffffff6; /* uh-oh! this will overflow after 10 seconds */

    for (;;) {
        xQueueSendToBack(update_time_queue, &fake_clock_time, 0);
        vTaskDelay(1000/portTICK_RATE_MS);
        fake_clock_time++;
    }
}

/* Task that updates the global time variable when a new time reading is
 * received */
void update_time_task(void *args)
{
    xQueueHandle update_time_queue = (xQueueHandle)args;
    int time_read;

    led_init();
    led_set(0, 0);
    led_set(1, 0);

    for (;;) {
        xQueueReceive(update_time_queue, &time_read, portMAX_DELAY);
        curr_time = time_read;
        led_set(0, curr_time % 2);
    }
}

int main(void)
{
    /* The communication channel between the two tasks is simply a one-element
       queue */
    xQueueHandle update_time_queue = xQueueCreate(1, sizeof(int));
    /* update_queue = xQueueCreate(128, sizeof(int32_t)); */

    tower_entry();

    /* The checker task.  Defined in checker_task.c */
    /* xTaskCreate(verify_updates, (signed char *)"verify_updates", 1000, update_queue, 100, &checker_task_handle); */

    xTaskCreate(read_clock_task, (signed char *)"read_clock_task", 1000, update_time_queue, 1, &read_clock_task_handle);

    xTaskCreate(update_time_task, (signed char *)"update_time_task", 1000, update_time_queue, 0, &update_time_task_handle);

    vTaskStartScheduler();
    for(;;);

    return 0;
}
