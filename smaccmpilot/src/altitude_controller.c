#include <string.h>
#include <smaccmpilot/altitude_controller.h>

static float alt_hold_throttle( const struct position_estimate *pos_estimate,
                                float user_throttle )
{
    return user_throttle;
}

void altitude_compensate(const struct position_estimate *pos,
                         const struct userinput_result *userin,
                         struct userinput_result *out)
{
    memcpy(out, userin, sizeof(struct userinput_result)); /* punt for now */
    if (userin->mode == 1 || userin->mode == 2) { /* alt hold or loiter */
        out->throttle = alt_hold_throttle(pos, userin->throttle);
    }
}
