
#include "gcs_transmit.h"
#include "gcs_transmit_driver.h"

static portTickType gcs_transmit_schedule_streams(void);
static void gcs_transmit_stream_due(int stream);
static void gcs_transmit_send_streams();

struct gcs_transmit_action {
    int stream;
    uint16_t rate;
    uint32_t last;
};

static struct gcs_timed_action g_actions [] = {
    { GCS_TRANSMIT_STREAM_HEARTBEAT       , 1, 0 },
    { GCS_TRANSMIT_STREAM_SERVO_OUTPUT_RAW, 0, 0 },
    { GCS_TRANSMIT_STREAM_ATTITUDE        , 0, 0 },
    { GCS_TRANSMIT_STREAM_GPS_RAW_INT     , 0, 0 },
    { GCS_TRANSMIT_STREAM_VFR_HUD         , 0, 0 },
    { 0                                   , 0, 0 }
};

static bool g_stream_due[GCS_TRANSMIT_NUM_STREAMS] = {0};

static void gcs_transmit_stream_due(int stream) {
    if (stream < 1 || stream >= GCS_TRANSMIT_NUM_STREAMS)
        return;
    g_stream_due[stream] = true;
}

void gcs_transmit_set_stream_rate(int stream, bool enable, uint16_t rate_hz) {
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
}

static portTickType gcs_transmit_schedule_streams(void) {
    portTickType now = xTaskGetTickCount();
    portTickType result = portMAX_DELAY;

    for (int i = 0; g_actions[i].stream > 0; ++i) {
        /* skip disabled actions */
        if (g_actions[i].rate == 0)
            continue;
    
        portTickType rate_ticks = 1000 / g_actions[i].rate;
        portTickType due = g_actions[i].last + rate_ticks;

        if (now >= due) {
            g_actions[i].last = now;
            gcs_transmit_stream_due(g_actions[i].stream);

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
    return result;
}

static void gcs_transmit_send_streams(void) {
    for(i = 1; i < GCS_TRANSMIT_NUM_STREAMS; ++i) {
        if (g_stream_due[i]) {
            switch (i) {
                case GCS_TRANSMIT_STREAM_HEARTBEAT:
                break;
                case GCS_TRANSMIT_STREAM_SERVO_OUTPUT_RAW:
                break;
                case GCS_TRANSMIT_STREAM_ATTITUDE:
                break;
                case GCS_TRANSMIT_STREAM_GPS_RAW_INT:
                break;
                case GCS_TRANSMIT_STREAM_VFR_HUD:
                break;
            }
        }
    }
}
