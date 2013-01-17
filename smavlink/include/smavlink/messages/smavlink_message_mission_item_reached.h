#ifndef __SMAVLINK_MESSAGE_MISSION_ITEM_REACHED_H__
#define __SMAVLINK_MESSAGE_MISSION_ITEM_REACHED_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct mission_item_reached_msg {
    uint16_t mission_item_reached_seq;
};
void smavlink_send_mission_item_reached(struct mission_item_reached_msg* var0,
                                        struct smavlink_out_channel* var1,
                                        struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MISSION_ITEM_REACHED_H__ */