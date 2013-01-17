#ifndef __SMAVLINK_MESSAGE_ROLL_PITCH_YAW_SPEED_THRUST_SETPOINT_H__
#define __SMAVLINK_MESSAGE_ROLL_PITCH_YAW_SPEED_THRUST_SETPOINT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct roll_pitch_yaw_speed_thrust_setpoint_msg {
    uint32_t time_boot_ms;
    float roll_speed;
    float pitch_speed;
    float yaw_speed;
    float thrust;
};
void smavlink_send_roll_pitch_yaw_speed_thrust_setpoint(struct roll_pitch_yaw_speed_thrust_setpoint_msg* var0,
                                                        struct smavlink_out_channel* var1,
                                                        struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_ROLL_PITCH_YAW_SPEED_THRUST_SETPOINT_H__ */