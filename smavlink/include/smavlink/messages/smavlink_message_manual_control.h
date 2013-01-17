#ifndef __SMAVLINK_MESSAGE_MANUAL_CONTROL_H__
#define __SMAVLINK_MESSAGE_MANUAL_CONTROL_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct manual_control_msg {
    int16_t x;
    int16_t y;
    int16_t z;
    int16_t r;
    uint16_t buttons;
    uint8_t target;
};
void smavlink_send_manual_control(struct manual_control_msg* var0,
                                  struct smavlink_out_channel* var1,
                                  struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MANUAL_CONTROL_H__ */