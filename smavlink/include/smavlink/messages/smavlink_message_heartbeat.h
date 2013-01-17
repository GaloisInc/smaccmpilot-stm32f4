#ifndef __SMAVLINK_MESSAGE_HEARTBEAT_H__
#define __SMAVLINK_MESSAGE_HEARTBEAT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct heartbeat_msg {
    uint32_t custom_mode;
    uint8_t mavtype;
    uint8_t autopilot;
    uint8_t base_mode;
    uint8_t system_status;
    uint8_t mavlink_version;
};
void smavlink_send_heartbeat(struct heartbeat_msg* var0,
                             struct smavlink_out_channel* var1,
                             struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_HEARTBEAT_H__ */