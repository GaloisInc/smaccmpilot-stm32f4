#ifndef __SMAVLINK_MESSAGE_HIL_STATE_H__
#define __SMAVLINK_MESSAGE_HIL_STATE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct hil_state_msg {
    uint64_t time_usec;
    float roll;
    float pitch;
    float yaw;
    float rollspeed;
    float pitchspeed;
    float yawspeed;
    int32_t lat;
    int32_t lon;
    int32_t alt;
    int16_t vx;
    int16_t vy;
    int16_t vz;
    int16_t xacc;
    int16_t yacc;
    int16_t zacc;
};
void smavlink_send_hil_state(struct hil_state_msg* var0,
                             struct smavlink_out_channel* var1,
                             struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_HIL_STATE_H__ */