#ifndef __SMAVLINK_MESSAGE_DEBUG_VECT_H__
#define __SMAVLINK_MESSAGE_DEBUG_VECT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct debug_vect_msg {
    uint64_t time_usec;
    float x;
    float y;
    float z;
    uint8_t name[10U];
};
void smavlink_send_debug_vect(struct debug_vect_msg* var0,
                              struct smavlink_out_channel* var1,
                              struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_DEBUG_VECT_H__ */