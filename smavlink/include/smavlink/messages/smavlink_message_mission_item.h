#ifndef __SMAVLINK_MESSAGE_MISSION_ITEM_H__
#define __SMAVLINK_MESSAGE_MISSION_ITEM_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct mission_item_msg {
    float param1;
    float param2;
    float param3;
    float param4;
    float x;
    float y;
    float z;
    uint16_t mission_item_seq;
    uint16_t command;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t frame;
    uint8_t current;
    uint8_t autocontinue;
};
void smavlink_send_mission_item(struct mission_item_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MISSION_ITEM_H__ */