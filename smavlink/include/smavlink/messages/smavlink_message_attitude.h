#ifndef __SMAVLINK_MESSAGE_ATTITUDE_H__
#define __SMAVLINK_MESSAGE_ATTITUDE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct attitude_msg {
    uint32_t time_boot_ms;
    float roll;
    float pitch;
    float yaw;
    float rollspeed;
    float pitchspeed;
    float yawspeed;
};
void smavlink_send_attitude(struct attitude_msg* var0,
                            struct smavlink_out_channel* var1,
                            struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_ATTITUDE_H__ */