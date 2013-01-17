#ifndef __SMAVLINK_MESSAGE_LOCAL_POSITION_NED_H__
#define __SMAVLINK_MESSAGE_LOCAL_POSITION_NED_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct local_position_ned_msg {
    uint32_t time_boot_ms;
    float x;
    float y;
    float z;
    float vx;
    float vy;
    float vz;
};
void smavlink_send_local_position_ned(struct local_position_ned_msg* var0,
                                      struct smavlink_out_channel* var1,
                                      struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_LOCAL_POSITION_NED_H__ */