#ifndef __SMAVLINK_MESSAGE_COMMAND_LONG_H__
#define __SMAVLINK_MESSAGE_COMMAND_LONG_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct command_long_msg {
    float param1;
    float param2;
    float param3;
    float param4;
    float param5;
    float param6;
    float param7;
    uint16_t command;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t confirmation;
};
void smavlink_send_command_long(struct command_long_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_COMMAND_LONG_H__ */