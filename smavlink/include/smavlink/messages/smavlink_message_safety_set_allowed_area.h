#ifndef __SMAVLINK_MESSAGE_SAFETY_SET_ALLOWED_AREA_H__
#define __SMAVLINK_MESSAGE_SAFETY_SET_ALLOWED_AREA_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct safety_set_allowed_area_msg {
    float p1x;
    float p1y;
    float p1z;
    float p2x;
    float p2y;
    float p2z;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t frame;
};
void smavlink_send_safety_set_allowed_area(struct safety_set_allowed_area_msg* var0,
                                           struct smavlink_out_channel* var1,
                                           struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SAFETY_SET_ALLOWED_AREA_H__ */