
#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <hwf4/usart.h>

#include <include/mavlink/v1.0/mavlink_types.h>
#include <include/mavlink/v1.0/common/mavlink.h>

#include "smaccmpilot/gcs_receive.h"

#define GCS_UART usart1

/* HIL state */
static xSemaphoreHandle hilstate_mutex;
static struct sensors_result shared_hilstate_sensors;
static struct position_result shared_hilstate_position;
static void gcs_receive_set_hilstate(const struct sensors_result *sensors,
                                     const struct position_result *position );

/* lets not use the HAL. */
static void panic(const char* panicmsg);

/* GCS Receive task */
static xTaskHandle gcs_receive_taskhandle;
static void gcs_receive_task(void *arg);
static void gcs_receive_handler(const mavlink_message_t *msg);

/* Handled Messages */
static void gcs_receive_handle_request_datastream(const mavlink_message_t *msg);
static void gcs_receive_handle_hil_state(const mavlink_message_t *msg);
static void gcs_receive_handle_unknown(const mavlink_message_t *msg);

/* Keep a history of unknown message ids caught by handle_unknown */
#define UNKNOWN_ID_HISTORY_LEN 256
static uint8_t handle_unknown_id_history[UNKNOWN_ID_HISTORY_LEN] = {0};
static uint8_t handle_unknown_id_history_idx = 0;

/* Entry point */
void gcs_receive_init(void) {
    hilstate_mutex = xSemaphoreCreateMutex();
    xTaskCreate(gcs_receive_task, (signed char *)"gcsr", 1024, NULL, 1,
                &gcs_receive_taskhandle);
}

/* Memory guarded interface to retrieve hilstate */
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
        panic("PANIC: gcs_receive_get_hilstate took too long "
            "to take memory barrier\r\n");
    }

    return result;
}

/* Memory guarded interface to set hilstate */
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


static void gcs_receive_task(void *arg) {
    mavlink_message_t msg;
    mavlink_status_t status;
    ssize_t count;
    uint8_t byte;

    for (;;) {
        count = usart_read_timeout(GCS_UART, portMAX_DELAY, &byte, 1);
        if (count > 0) {
            bool rxcomplete = mavlink_parse_char(0, byte, &msg, &status);
            if (rxcomplete) {
                gcs_receive_handler(&msg);
            }
        }
    }

}

static void gcs_receive_handler(const mavlink_message_t *msg) {
    switch (msg->msgid) {
        case MAVLINK_MSG_ID_HIL_STATE:
            gcs_receive_handle_hil_state(msg);
            break;
        case MAVLINK_MSG_ID_REQUEST_DATA_STREAM:
            gcs_receive_handle_request_datastream(msg);
            break;
        default:
            gcs_receive_handle_unknown(msg);
            break;
    }
}


static void gcs_receive_handle_hil_state(const mavlink_message_t *msg) {
    struct sensors_result sensors;
    struct position_result position;

    uint32_t now = xTaskGetTickCount();

    mavlink_hil_state_t m;
    mavlink_msg_hil_state_decode(msg, &m);

    sensors.valid    = true;
    sensors.roll     = m.roll;
    sensors.pitch    = m.pitch;
    sensors.yaw      = m.yaw;
    sensors.omega_x  = m.rollspeed;
    sensors.omega_y  = m.pitchspeed;
    sensors.omega_z  = m.yawspeed;
    sensors.baro_alt = (float) m.alt / 1000.0f;
    sensors.xacc     = m.xacc;
    sensors.yacc     = m.yacc;
    sensors.zacc     = m.zacc;
    sensors.time     = now;

    position.lat     = m.lat;
    position.lon     = m.lon;
    position.gps_alt = m.alt;
    position.vx      = m.vx;
    position.vy      = m.vy;
    position.vz      = m.vz;
    position.time    = now;

    gcs_receive_set_hilstate(&sensors, &position);

}

static void gcs_receive_handle_request_datastream(const mavlink_message_t *msg){
    /* XXX punt on this for now */
}

static void gcs_receive_handle_unknown(const mavlink_message_t *msg) {
    handle_unknown_id_history[handle_unknown_id_history_idx] = msg->msgid;
    handle_unknown_id_history_idx =
        (handle_unknown_id_history_idx + 1) % UNKNOWN_ID_HISTORY_LEN;
}

static void panic(const char* panicmsg) {
        usart_write(GCS_UART, (const uint8_t*)panicmsg, strlen(panicmsg));
        for(;;);
}
