#ifndef __SMAVLINK_MESSAGE_STATUSTEXT_H__
#define __SMAVLINK_MESSAGE_STATUSTEXT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct statustext_msg {
    uint8_t severity;
    uint8_t text[50U];
};
void smavlink_send_statustext(struct statustext_msg* var0,
                              struct smavlink_out_channel* var1,
                              struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_STATUSTEXT_H__ */