#ifndef __SMAVLINK_MESSAGE_CHANGE_OPERATOR_CONTROL_H__
#define __SMAVLINK_MESSAGE_CHANGE_OPERATOR_CONTROL_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct change_operator_control_msg {
    uint8_t target_system;
    uint8_t control_request;
    uint8_t version;
    uint8_t passkey[25U];
};
void smavlink_send_change_operator_control(struct change_operator_control_msg* var0,
                                           struct smavlink_out_channel* var1,
                                           struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_CHANGE_OPERATOR_CONTROL_H__ */