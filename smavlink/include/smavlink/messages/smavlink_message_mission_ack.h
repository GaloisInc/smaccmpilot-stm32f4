#ifndef __SMAVLINK_MESSAGE_MISSION_ACK_H__
#define __SMAVLINK_MESSAGE_MISSION_ACK_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct mission_ack_msg {
    uint8_t target_system;
    uint8_t target_component;
    uint8_t mission_ack_type;
};
void smavlink_send_mission_ack(struct mission_ack_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MISSION_ACK_H__ */