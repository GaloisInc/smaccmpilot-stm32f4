
#include "gcs.h"

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <smavlink/channel.h>
#include <smavlink/system.h>
#include <smavlink/messages/smavlink_message_heartbeat.h>
#include <smavlink/send.h>

#include <AP_HAL.h>
extern const AP_HAL::HAL& hal;

static xTaskHandle gcs_task_handle;
static xSemaphoreHandle gcs_mutex;
static struct gcs_state gcs_shared_state;

static smavlink_out_channel smav_ch;
static smavlink_system smav_sys;

/* Control thread, shares state */
static void gcs_task(void* args);
static void gcs_get(struct gcs_state *state);

/* Manage smavlink objects */
static void gcs_setup();
static void gcs_send_heartbeat(const struct gcs_state *state);

/* smavlink write delegation */
static size_t gcs_write(void* delegate, const uint8_t *data, size_t len);
static bool   gcs_begin_atomic(void*, size_t);
static void   gcs_end_atomic(void*);

void gcs_init(void) {
    gcs_mutex = xSemaphoreCreateMutex();
    xTaskCreate(gcs_task, (signed char *)"gcs", 1024, NULL, 0,
                &gcs_task_handle);
}

static void gcs_task(void* args) {

    struct gcs_state state = {0};

    gcs_setup();

    portTickType last_wake_time = xTaskGetTickCount();

    for(;;) {
        vTaskDelayUntil(&last_wake_time, 1000);
        gcs_get(&state);
        gcs_send_heartbeat(&state);
    }
}

/* gcs_get: for internal thread to grab the shared state */
static void gcs_get(struct gcs_state *state) {
    if (xSemaphoreTake(gcs_mutex, 1)) {
        memcpy(state, &gcs_shared_state,
                sizeof(struct gcs_state));
        xSemaphoreGive(gcs_mutex);
    } else {
        hal.scheduler->panic("PANIC: gcs_get took too long to grab "
                "memory barrier (should never happen)");
    }
}

/* gcs_set: for external threads to update the shared state */
void gcs_set(const struct gcs_state *out) {
    if (xSemaphoreTake(gcs_mutex, 1)) {
        memcpy(&gcs_shared_state, out,
                sizeof(struct gcs_state));
        xSemaphoreGive(gcs_mutex);
    } else {
        hal.scheduler->panic("PANIC: gcs_set took too long to grab "
                "memory barrier (should never happen).");
    }
}

static void gcs_setup() {
    /* Setup channel */
    smav_ch.tx_seq         = 0; 
    smav_ch.write_delegate = NULL;
    smav_ch.begin_atomic   = gcs_begin_atomic;
    smav_ch.end_atomic     = gcs_end_atomic;
    smav_ch.write          = gcs_write;

    /* Setup system */
    smav_sys.sysid  = 1;
    smav_sys.compid = 0;

    hal.console->println("gcs setup complete");
}

static size_t gcs_write(void* delegate, const uint8_t *data, size_t len) {
    hal.uartA->write(data, len);
    return len;
}

static bool gcs_begin_atomic(void* delegate, size_t len) {
    /* trivial implementation for now */
    return true;
}

static void gcs_end_atomic(void*) {
    /* trivial implementation for now */
}

static void gcs_send_heartbeat(const struct gcs_state *state) {
    /* the following are MAV_MODE_FLAG_s*/
    const uint8_t STABILIZE_ENABLED = 16;
    const uint8_t SAFETY_ARMED = 128;
    uint8_t basemode = STABILIZE_ENABLED | (state->armed? SAFETY_ARMED : 0);
    struct heartbeat_msg msg = {
        /* .type: */          2, /* Quadrotor*/
        /* .autopilot: */     0, /* MAV_AUTOPILOT_GENERIC */
        /* .base_mode: */     basemode,
        /* .custom_mode: */   0,
        /* .system_status: */ 4, /* MAV_STATE_ACTIVE */
        /* .mavlink_vers: */  3  /* magic num - must always be 3 */
    };
    smavlink_send_heartbeat(&msg, &smav_ch, &smav_sys);
}

