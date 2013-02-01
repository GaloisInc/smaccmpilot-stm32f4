#include <string.h>
#include <smaccmpilot/altitude_controller.h>
#include <smaccmpilot/pid_stabilize.h>

#define MAX_CLIMB_RATE  2.0f

/* throttle cruise determined empirically for AR drone with flow system,
 * no foam guards, full battery. pch 01feb2013 */
static float g_throttle_cruise = 0.64f;
static float g_throttle_avg    = 0.0f;

static struct PID g_pi_throttle_climb_rate = {
    1.0f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // d_gain
    0.0f,                       // i_state
    -10.0f,                     // i_min
    10.0f,                      // i_max
    0.0f,                       // d_state
    1,                          // reset
};

/* ------------------------------------------------------------------------ */

static void update_throttle_cruise(float throttle,
                                   const struct position_estimate *pos,
                                   const struct sensors_result *sensors);

float throttle_to_climb_rate(float stick);


/* ------------------------------------------------------------------------ */

float get_throttle_cruise_estimate(void) {
    return g_throttle_cruise;
}

static float degrees(float rad) {
    return rad * 57.295779513082320876798154814105;
}

static float alt_hold_throttle( const struct position_estimate *pos_estimate,
                                float user_throttle )
{
    if (pos_estimate->vert_conf == 5) {
        float desired_rate = throttle_to_climb_rate(user_throttle);
        float actual_rate  = pos_estimate->vz;
        float error        = desired_rate - actual_rate;
        float result       = pid_update(&g_pi_throttle_climb_rate, error, actual_rate);
        return g_throttle_cruise + result;
    } else {
        return user_throttle;
   }
}

void altitude_compensate(const struct position_estimate *pos,
                         const struct sensors_result *sensors,
                         const struct userinput_result *userin,
                         struct userinput_result *out)
{
    if (userin->mode == 1 || userin->mode == 2) { /* alt hold or loiter */
        /* pass through userinput to output */
        memcpy(out, userin, sizeof(struct userinput_result));
        /* modify only the throttle */
        out->throttle = alt_hold_throttle(pos, userin->throttle);
    } else {
        /* user has control of throttle - monitor this to guess cruise
         * throttle */
        update_throttle_cruise(userin->throttle, pos, sensors);
        /* pass through user input to output */
        memcpy(out, userin, sizeof(struct userinput_result));
    }
}


static void update_throttle_cruise(float throttle,
                                   const struct position_estimate *pos,
                                   const struct sensors_result *sensors)
{
    if (g_throttle_avg == 0.0f) {
        g_throttle_avg = g_throttle_cruise;
    }

    // Calculate average throttle if we are in a level hover.
    if (throttle > 0.2 &&          /* throttle is high enough to be flying */
        pos->vert_conf > 5 &&      /* vertical velocity estimate is confident */
        fabsf(pos->vz) < 0.1 &&    /* vertical velocity is below 0.1m/s */
        fabsf(degrees(sensors->roll)) < 5.0f &&  /* pitch, roll are small */
        fabsf(degrees(sensors->pitch)) < 5.0f) {
        g_throttle_avg = g_throttle_avg * 0.99f + throttle * 0.01f;
        g_throttle_cruise = g_throttle_avg;
    }
}


// Convert a normalized throttle stick value [0.0, 1.0] to a climb
// rate in m/sec.  This uses a wide dead zone to maintain a constant
// altitude.
float throttle_to_climb_rate(float stick)
{
    const float dead_zone = 0.4f;
    const float cutoff    = 0.9f;

    stick = (stick * 2.0f) - 1.0f; // convert to [-1.0, 1.0]

    if (stick >= cutoff) {
        return MAX_CLIMB_RATE;
    } else if (stick <= -cutoff) {
        return 0.0f;
    } else if (stick >= dead_zone) {
        stick = (stick - dead_zone) * (1.0f / (1.0f - dead_zone));
        return stick * MAX_CLIMB_RATE;
    } else if (stick <= -dead_zone) {
        stick = (stick + dead_zone) * (1.0f / (1.0f - dead_zone));
        return stick * MAX_CLIMB_RATE;
    } else {
        return 0.0f;
    }
}
