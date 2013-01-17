#ifndef __SMAVLINK_MESSAGE_COMMAND_ACK_H__
#define __SMAVLINK_MESSAGE_COMMAND_ACK_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct command_ack_msg {
    uint16_t command;
    uint8_t result;
};
void smavlink_send_command_ack(struct command_ack_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_COMMAND_ACK_H__ */