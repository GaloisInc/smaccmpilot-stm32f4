
#include <string.h>
#include <stdbool.h>

#include <hwf4/usart.h>

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <smavlink/channel.h>
#include <smavlink/system.h>

#include "gcs_transmit.h"
#include "gcs_transmit_driver.h"

#ifdef CONFIG_GCS_UART
# define GCS_TRANSMIT_USART CONFIG_GCS_UART
#else
# define GCS_TRANSMIT_USART usart1
#endif

/* lets not use the HAL. */
static void panic(const char* panicmsg);

static portTickType gcs_transmit_schedule_streams(bool* streams_due);
static void gcs_transmit_send_streams(bool* streams_due,
                                      struct smavlink_out_channel* ch,
                                      struct smavlink_system *sys);

static xTaskHandle gcs_transmit_task_handle;
static void gcs_transmit_task(void* args);

static xSemaphoreHandle shared_state_mutex;
static struct sensors_result     shared_sensors;
static struct position_result    shared_position;
static struct motorsoutput_result shared_motors;
static struct servo_result       shared_servo;
static struct userinput_result   shared_user;

static struct smavlink_out_channel ch;
static struct smavlink_system sys;

static size_t gcstx_write(void* delegate, const uint8_t *data, size_t len);
static bool   gcstx_begin_atomic(void*, size_t);
static void   gcstx_end_atomic(void*);


struct gcs_timed_action {
    int stream;
    uint16_t rate;
    uint32_t last;
};

static xSemaphoreHandle g_action_mutex;
static struct gcs_timed_action g_actions [] = {
    { GCS_TRANSMIT_STREAM_HEARTBEAT       , 1, 0 },
    { GCS_TRANSMIT_STREAM_SERVO_OUTPUT_RAW, 0, 0 },
    { GCS_TRANSMIT_STREAM_ATTITUDE        , 0, 0 },
    { GCS_TRANSMIT_STREAM_GPS_RAW_INT     , 0, 0 },
    { GCS_TRANSMIT_STREAM_VFR_HUD         , 0, 0 },
    { GCS_TRANSMIT_STREAM_GLOBAL_POSITION_INT, 0, 0 },
    { GCS_TRANSMIT_STREAM_PARAMS          , 1, 0 },
    { 0                                   , 0, 0 }
};


void gcs_transmit_init(void) {
    shared_state_mutex = xSemaphoreCreateMutex();
    g_action_mutex = xSemaphoreCreateMutex();
}

void gcs_transmit_start_task(void) {
    xTaskCreate(gcs_transmit_task, (signed char*)"gcst", 512, NULL, 0,
            &gcs_transmit_task_handle);
}

struct smavlink_out_channel *gcs_transmit_get_channel(void) {
    return &ch;
}

struct smavlink_system *gcs_transmit_get_system(void) {
    return &sys;
}

static void gcs_transmit_task(void* args) {
    bool streams_due[GCS_TRANSMIT_NUM_STREAMS];

    /* setup channel */
    ch.tx_seq = 0;
    ch.write_delegate = NULL;
    ch.begin_atomic = gcstx_begin_atomic;
    ch.end_atomic = gcstx_end_atomic;
    ch.write = gcstx_write;

    /* setup system */
    sys.sysid = 1;
    sys.compid = 0;

    portTickType last_wake_time = xTaskGetTickCount();
    portTickType dt = 10;

    for(;;) {
        vTaskDelayUntil(&last_wake_time, dt);

        for(int i = 0; i < GCS_TRANSMIT_NUM_STREAMS; ++i) {
            streams_due[i] = false;
        }

        dt = gcs_transmit_schedule_streams(streams_due);

        gcs_transmit_send_streams(streams_due, &ch, &sys);
    }

    for(;;);
}

void gcs_transmit_set_states( const struct sensors_result *sensors,
                              const struct position_result *position,
                              const struct motorsoutput_result *motors,
                              const struct servo_result *servo,
                              const struct userinput_result *user )
{
    if (xSemaphoreTake(shared_state_mutex, 1)) {
        memcpy(&shared_sensors,  sensors,  sizeof(struct sensors_result));
        memcpy(&shared_position, position, sizeof(struct position_result));
        memcpy(&shared_motors,   motors,   sizeof(struct motorsoutput_result));
        memcpy(&shared_servo,    servo,    sizeof(struct servo_result));
        memcpy(&shared_user,     user,     sizeof(struct userinput_result));
        xSemaphoreGive(shared_state_mutex);
    } else {
        panic("PANIC: gcs_transmit_set_states took too long to "
                "get memory barrier");
    }
}

void gcs_transmit_get_states( struct sensors_result *sensors,
                              struct position_result *position,
                              struct motorsoutput_result *motors,
                              struct servo_result *servo,
                              struct userinput_result *user )
{
    if (xSemaphoreTake(shared_state_mutex, 1)) {
        memcpy(sensors,  &shared_sensors,  sizeof(struct sensors_result));
        memcpy(position, &shared_position, sizeof(struct position_result));
        memcpy(motors,   &shared_motors,   sizeof(struct motorsoutput_result));
        memcpy(servo,    &shared_servo,    sizeof(struct servo_result));
        memcpy(user,     &shared_user,     sizeof(struct userinput_result));
        xSemaphoreGive(shared_state_mutex);
    } else {
        panic("PANIC: gcs_transmit_get_states took too long to "
                "get memory barrier");
    }
}


void gcs_transmit_set_stream_rate(int stream, bool enable, uint16_t rate_hz) {
    if (xSemaphoreTake(g_action_mutex, 1)) {
        for (int i = 0; g_actions[i].stream > 0; ++i) {
            if ((stream == 0 && g_actions[i].stream != 0) ||
                (stream != 0 && g_actions[i].stream == stream)) {
                if (enable) {
                    g_actions[i].rate = rate_hz;
                } else {
                    g_actions[i].rate = 0;
                }
            }
        }
        xSemaphoreGive(g_action_mutex);
    } else {
        panic("PANIC: gcs_transmit_set_stream_rate took too long to "
                "get memory barrier");
    }
}

static portTickType gcs_transmit_schedule_streams(bool* streams_due) {
    portTickType now = xTaskGetTickCount();
    portTickType result = portMAX_DELAY;

    if (xSemaphoreTake(g_action_mutex, 1)) {
        for (int i = 0; g_actions[i].stream > 0; ++i) {
            /* skip disabled actions */
            if (g_actions[i].rate == 0)
                continue;
        
            portTickType rate_ticks = 1000 / g_actions[i].rate;
            portTickType due = g_actions[i].last + rate_ticks;

            if (now >= due) {
                g_actions[i].last = now;
                streams_due[g_actions[i].stream] = true;

                /* calculate next time for this stream */
                if (due + rate_ticks > now) {
                    portTickType dt = (due + rate_ticks) - now;
                    if (dt < result) {
                        /* accumulate the earliest deadline for next stream */
                        result = dt;
                    }
                } else {
                    /* due again immediately */
                    result = 0;
                }
            } else {
                portTickType dt = due - now;
                if (dt < result ) {
                    /* accumulate the earliest deadline for next stream */
                    result = dt;
                }
            }
        }
        xSemaphoreGive(g_action_mutex);
    } else {
        panic("PANIC: gcs_transmit_schedule_streams took too long to "
                "get memory barrier");
    }
    return result;
}

static void gcs_transmit_send_streams( bool *streams_due,
                                       struct smavlink_out_channel *ch,
                                       struct smavlink_system *sys ) {
    struct sensors_result sensors;
    struct position_result position;
    struct motorsoutput_result motors;
    struct servo_result servo;
    struct userinput_result userinput;
    
    gcs_transmit_get_states( &sensors, &position, &motors, &servo, &userinput );

    if (streams_due[GCS_TRANSMIT_STREAM_HEARTBEAT]) {
        gcs_transmit_send_heartbeat(&motors, &userinput, ch, sys);
    }

    if (streams_due[GCS_TRANSMIT_STREAM_SERVO_OUTPUT_RAW]) {
        gcs_transmit_send_servo_output(&servo, &userinput, ch, sys);
    }

    if (streams_due[GCS_TRANSMIT_STREAM_ATTITUDE]) {
        gcs_transmit_send_attitude(&sensors, ch, sys);
    }

    if (streams_due[GCS_TRANSMIT_STREAM_GPS_RAW_INT]) {
        gcs_transmit_send_gps_raw_int(&position, ch, sys);
    }

    if (streams_due[GCS_TRANSMIT_STREAM_VFR_HUD]) {
        gcs_transmit_send_vfrhud(&position, &motors, &sensors, ch, sys);
    }
 
    if (streams_due[GCS_TRANSMIT_STREAM_GLOBAL_POSITION_INT]) {
        gcs_transmit_send_global_position_int(&position, &sensors, ch, sys);
    }

    if (streams_due[GCS_TRANSMIT_STREAM_PARAMS]) {
        gcs_transmit_send_params(ch, sys);
    }
}

static size_t gcstx_write(void* delegate, const uint8_t *data, size_t len) {
    usart_write(GCS_TRANSMIT_USART, data, len);
    return len;
}

static bool   gcstx_begin_atomic(void* delegate, size_t len) {
    return true; /* trivial */
}

static void   gcstx_end_atomic(void* delegate) {
    /* trivial */
}

static void panic(const char* panicmsg) {
    usart_write(GCS_TRANSMIT_USART, (const uint8_t*)panicmsg, strlen(panicmsg));
    for(;;);
}

