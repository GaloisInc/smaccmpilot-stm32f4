#ifndef __SMAVLINK_MESSAGE_SYSTEM_TIME_H__
#define __SMAVLINK_MESSAGE_SYSTEM_TIME_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct system_time_msg {
    uint64_t time_unix_usec;
    uint32_t time_boot_ms;
};
void smavlink_send_system_time(struct system_time_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SYSTEM_TIME_H__ */