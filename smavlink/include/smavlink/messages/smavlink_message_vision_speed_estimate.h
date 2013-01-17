#ifndef __SMAVLINK_MESSAGE_VISION_SPEED_ESTIMATE_H__
#define __SMAVLINK_MESSAGE_VISION_SPEED_ESTIMATE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct vision_speed_estimate_msg {
    uint64_t usec;
    float x;
    float y;
    float z;
};
void smavlink_send_vision_speed_estimate(struct vision_speed_estimate_msg* var0,
                                         struct smavlink_out_channel* var1,
                                         struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_VISION_SPEED_ESTIMATE_H__ */