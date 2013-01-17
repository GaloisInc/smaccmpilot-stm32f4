#ifndef __SMAVLINK_MESSAGE_PING_H__
#define __SMAVLINK_MESSAGE_PING_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct ping_msg {
    uint64_t time_usec;
    uint32_t ping_seq;
    uint8_t target_system;
    uint8_t target_component;
};
void smavlink_send_ping(struct ping_msg* var0,
                        struct smavlink_out_channel* var1,
                        struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_PING_H__ */