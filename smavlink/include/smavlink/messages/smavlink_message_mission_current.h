#ifndef __SMAVLINK_MESSAGE_MISSION_CURRENT_H__
#define __SMAVLINK_MESSAGE_MISSION_CURRENT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct mission_current_msg {
    uint16_t mission_current_seq;
};
void smavlink_send_mission_current(struct mission_current_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MISSION_CURRENT_H__ */