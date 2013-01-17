#ifndef __SMAVLINK_MESSAGE_AUTH_KEY_H__
#define __SMAVLINK_MESSAGE_AUTH_KEY_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct auth_key_msg {
    uint8_t key[32U];
};
void smavlink_send_auth_key(struct auth_key_msg* var0,
                            struct smavlink_out_channel* var1,
                            struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_AUTH_KEY_H__ */