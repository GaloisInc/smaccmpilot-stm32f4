
#include "gcs_receive.h"

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <include/mavlink/v1.0/mavlink_types.h>
#include <include/mavlink/v1.0/common/mavlink.h>

/* HIL state */
static xSemaphoreHandle hilstate_mutex;
static struct sensors_result shared_hilstate_sensors;
static struct sensors_position shared_hilstate_position;
static void gcs_receive_set_hilstate(const struct sensors_result *sensors,
                                     const struct position_result *position );

/* lets not use the HAL. */
static void panic(const char* panicmsg);

// Read the sensor result.
bool gcs_receive_get_hilstate(struct sensors_result *sensors,
                              struct position_result *position )
{
    bool result = false;

    if (xSemaphoreTake(hilstate_mutex, 1)) {
        memcpy(sensors, &shared_hilstate_sensors,
                        sizeof(struct sensors_result));
        memcpy(position, &shared_hilstate_position,
                        sizeof(struct position_result));
        result = sensors->valid;
        xSemaphoreGive(hilstate_mutex);
    } else {
        panic("PANIC: gcs_receive_get_hilstate took too long"
            "to take memory barrier\r\n");
    }

    return result;
}

// Set the sensor result from inside the GCS thread.
static void gcs_receive_set_hilstate(const struct sensors_result *sensors,
                                     const struct position_result *position )
{
    if (xSemaphoreTake(hilstate_mutex, 1)) {
        memcpy(&shared_hilstate_sensors, sensors,
                        sizeof(struct sensors_result));
        memcpy(&shared_hilstate_position, position,
                        sizeof(struct position_result));
        xSemaphoreGive(hilstate_mutex);
    } else {
        panic("PANIC: gcs_receive_set_hilstate took too long "
                "to take memory barrier\r\n");
    }
}








static void panic(const char* panicmsg) {
        usart_write(GCS_UART, panicmsg, strlen(panicmsg));
        for(;;);
}
