#ifndef __SMAVLINK_MESSAGE_MISSION_COUNT_H__
#define __SMAVLINK_MESSAGE_MISSION_COUNT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct mission_count_msg {
    uint16_t count;
    uint8_t target_system;
    uint8_t target_component;
};
void smavlink_send_mission_count(struct mission_count_msg* var0,
                                 struct smavlink_out_channel* var1,
                                 struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MISSION_COUNT_H__ */