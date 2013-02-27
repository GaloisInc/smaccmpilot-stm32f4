
#include <FreeRTOS.h>
#include <task.h>

#include <smaccmpilot/position_estimator.h>

void position_estimate(const struct sensors_result *sensors,
                       const struct position_result *position,
                       const struct optflow_result *optflow,
                             struct position_estimate *est )
{
    uint32_t now = xTaskGetTickCount();
    uint32_t last_estimate = est->time;
    est->time = now;

    if (!optflow->valid || (now - optflow->time) > 200) {
        est->horiz_conf = 0;
        est->vert_conf = 0;
        return;
    }

    /* don't process the same optflow sample twice. Sample uniquely tagged with
     * usec stamp on sensor, into optflow->sensortime ) */
    if (optflow->sensortime == est->optflow_t) {
        return;
    } else {
        est->optflow_t = optflow->sensortime;
    }

    /* estimate vx, vy: low pass filter on optflow->flow_x, optflow->flow_y */
    if (optflow->quality > 40) {
        if (est->horiz_conf > 0) { 
            const float filter_time_const = 0.6f;
            const float state_x = est->vx;
            const float state_y = est->vy;
            const float input_x = optflow->flow_x;
            const float input_y = optflow->flow_y;
            est->vx = ((1.0f - filter_time_const) * input_x) +
                (filter_time_const * state_x);
            est->vy = ((1.0f - filter_time_const) * input_y) +
                (filter_time_const * state_y);
        } else {
            const float input_x = optflow->flow_x;
            const float input_y = optflow->flow_y;
            est->vx = input_x;
            est->vy = input_y;
        }

        if (est->horiz_conf < 10) {
            est->horiz_conf++;
        }

    } else {
        est->vx = 0;
        est->vy = 0;
        est->horiz_conf = 0;
    }

   
    const float ground_dist = optflow->ground_dist;
    if (ground_dist > 0.10f && ground_dist < 4.5f) {

        /* estimate alt, low passed ground_dist */
        {
            if (est->vert_conf > 0) {
                const float filter_time_const = 0.7f;
                const float state = est->alt;
                const float input = optflow->ground_dist;
                est->alt = ((1.0f - filter_time_const) * input) +
                        (filter_time_const * state);
            } else {
                const float input = optflow->ground_dist;
                est->alt = input;
            }
        }

        /* estimate vz, low passed derivative of alt */
        if (est->vert_conf > 0) {
            const uint32_t dt_millis = now - last_estimate;
            const float dt = dt_millis / 1000.0f;
            const float filter_time_const = 0.9f;
            const float state = est->vz;
            const float input = (optflow->ground_dist - est->alt) / dt;
            est->vz = ((1.0f - filter_time_const) * input) +
                (filter_time_const * state);
        } else {
            est->vz = 0;
        }

        if (est->vert_conf < 10) {
            est->vert_conf++;
        }

    } else {
        est->vz        = 0;
        est->alt       = 0;
        est->vert_conf = 0;
    }

    /* todos:
     * later we can look at adding baro to the equation, using optflow plus
     * inertial heading for an integrator to estimate x & y, and so on.
     */
}
                            
void position_estimate_output( const struct position_estimate *est,
                                     struct position_result *position)
{
    /* translate the position_estimate fields from meters, meters/second into
     * position_result units */
    float alt_mm  = est->alt * 1000.0f;
    float vx_cm_s = est->vx * 100.0f;
    float vy_cm_s = est->vy * 100.0f;
    float vz_cm_s = est->vz * 100.0f;

    /* lat/lon: used as a dirty hack to transmit my confidence numbers... */
    position->lat     = (int32_t) est->horiz_conf * 100;
    position->lon     = (int32_t) est->vert_conf * 100;
    position->gps_alt = (int32_t) alt_mm;
    position->vx      = (int16_t) vx_cm_s;
    position->vy      = (int16_t) vy_cm_s;
    position->vz      = (int16_t) vz_cm_s;
    position->time    = est->time;
}
