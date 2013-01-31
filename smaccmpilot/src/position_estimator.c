
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
    }

    /* estimate vx, vy: low pass filter on optflow->flow_x, optflow->flow_y */
    if (optflow->quality > 40) {
        /* TODO low pass these, resetting when conf = 0 */
        est->vx = optflow->flow_x;
        est->vy = optflow->flow_y;

        if (est->horiz_conf < 10) {
            est->horiz_conf++;
        }

    } else {
        est->vx = 0;
        est->vy = 0;
        est->horiz_conf = 0;
    }

   
    const float ground_dist = optflow->ground_dist;
    if (ground_dist > 0.30f && ground_dist < 4.5f) {

        /* estimate vz: TODO make this a low pass filter on $
         *  (optflow->ground_dist - alt) / (now - time) */
        if (est->vert_conf > 0) {
            uint32_t dt_millis = now - last_estimate;
            float dt = dt_millis / 1000.0f;
            est->vz = (optflow->ground_dist - est->alt) / dt;
        }

        /* estimate alt: 
         * TODO make this a low pass filter on optflow->ground_dist */
        est->alt = optflow->ground_dist;

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
