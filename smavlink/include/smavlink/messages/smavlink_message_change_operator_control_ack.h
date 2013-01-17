#ifndef __SMAVLINK_MESSAGE_CHANGE_OPERATOR_CONTROL_ACK_H__
#define __SMAVLINK_MESSAGE_CHANGE_OPERATOR_CONTROL_ACK_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct change_operator_control_ack_msg {
    uint8_t gcs_system_id;
    uint8_t control_request;
    uint8_t ack;
};
void smavlink_send_change_operator_control_ack(struct change_operator_control_ack_msg* var0,
                                               struct smavlink_out_channel* var1,
                                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_CHANGE_OPERATOR_CONTROL_ACK_H__ */