#ifndef __SMAVLINK_MESSAGE_NAV_CONTROLLER_OUTPUT_H__
#define __SMAVLINK_MESSAGE_NAV_CONTROLLER_OUTPUT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct nav_controller_output_msg {
    float nav_roll;
    float nav_pitch;
    float alt_error;
    float aspd_error;
    float xtrack_error;
    int16_t nav_bearing;
    int16_t target_bearing;
    uint16_t wp_dist;
};
void smavlink_send_nav_controller_output(struct nav_controller_output_msg* var0,
                                         struct smavlink_out_channel* var1,
                                         struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_NAV_CONTROLLER_OUTPUT_H__ */