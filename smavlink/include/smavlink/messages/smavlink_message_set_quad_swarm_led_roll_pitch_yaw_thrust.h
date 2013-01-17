#ifndef __SMAVLINK_MESSAGE_SET_QUAD_SWARM_LED_ROLL_PITCH_YAW_THRUST_H__
#define __SMAVLINK_MESSAGE_SET_QUAD_SWARM_LED_ROLL_PITCH_YAW_THRUST_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct set_quad_swarm_led_roll_pitch_yaw_thrust_msg {
    uint8_t group;
    uint8_t mode;
    int16_t roll[4U];
    int16_t pitch[4U];
    int16_t yaw[4U];
    uint16_t thrust[4U];
    uint8_t led_red[4U];
    uint8_t led_blue[4U];
    uint8_t led_green[4U];
};
void smavlink_send_set_quad_swarm_led_roll_pitch_yaw_thrust(struct set_quad_swarm_led_roll_pitch_yaw_thrust_msg* var0,
                                                            struct smavlink_out_channel* var1,
                                                            struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SET_QUAD_SWARM_LED_ROLL_PITCH_YAW_THRUST_H__ */