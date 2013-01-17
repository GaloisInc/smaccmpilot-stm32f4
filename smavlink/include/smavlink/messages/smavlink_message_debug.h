#ifndef __SMAVLINK_MESSAGE_DEBUG_H__
#define __SMAVLINK_MESSAGE_DEBUG_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct debug_msg {
    uint32_t time_boot_ms;
    float value;
    uint8_t ind;
};
void smavlink_send_debug(struct debug_msg* var0,
                         struct smavlink_out_channel* var1,
                         struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_DEBUG_H__ */