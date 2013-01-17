#ifndef __SMAVLINK_MESSAGE_SET_MODE_H__
#define __SMAVLINK_MESSAGE_SET_MODE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct set_mode_msg {
    uint32_t custom_mode;
    uint8_t target_system;
    uint8_t base_mode;
};
void smavlink_send_set_mode(struct set_mode_msg* var0,
                            struct smavlink_out_channel* var1,
                            struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SET_MODE_H__ */