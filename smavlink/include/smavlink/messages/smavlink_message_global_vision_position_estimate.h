#ifndef __SMAVLINK_MESSAGE_GLOBAL_VISION_POSITION_ESTIMATE_H__
#define __SMAVLINK_MESSAGE_GLOBAL_VISION_POSITION_ESTIMATE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct global_vision_position_estimate_msg {
    uint64_t usec;
    float x;
    float y;
    float z;
    float roll;
    float pitch;
    float yaw;
};
void smavlink_send_global_vision_position_estimate(struct global_vision_position_estimate_msg* var0,
                                                   struct smavlink_out_channel* var1,
                                                   struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_GLOBAL_VISION_POSITION_ESTIMATE_H__ */