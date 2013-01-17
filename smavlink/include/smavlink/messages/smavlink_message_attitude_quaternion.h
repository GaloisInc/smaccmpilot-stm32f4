#ifndef __SMAVLINK_MESSAGE_ATTITUDE_QUATERNION_H__
#define __SMAVLINK_MESSAGE_ATTITUDE_QUATERNION_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct attitude_quaternion_msg {
    uint32_t time_boot_ms;
    float q1;
    float q2;
    float q3;
    float q4;
    float rollspeed;
    float pitchspeed;
    float yawspeed;
};
void smavlink_send_attitude_quaternion(struct attitude_quaternion_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_ATTITUDE_QUATERNION_H__ */