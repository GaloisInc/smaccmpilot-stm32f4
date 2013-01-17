#ifndef __SMAVLINK_MESSAGE_SET_ROLL_PITCH_YAW_SPEED_THRUST_H__
#define __SMAVLINK_MESSAGE_SET_ROLL_PITCH_YAW_SPEED_THRUST_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct set_roll_pitch_yaw_speed_thrust_msg {
    float roll_speed;
    float pitch_speed;
    float yaw_speed;
    float thrust;
    uint8_t target_system;
    uint8_t target_component;
};
void smavlink_send_set_roll_pitch_yaw_speed_thrust(struct set_roll_pitch_yaw_speed_thrust_msg* var0,
                                                   struct smavlink_out_channel* var1,
                                                   struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SET_ROLL_PITCH_YAW_SPEED_THRUST_H__ */