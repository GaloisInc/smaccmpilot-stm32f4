#ifndef __SMAVLINK_MESSAGE_RC_CHANNELS_SCALED_H__
#define __SMAVLINK_MESSAGE_RC_CHANNELS_SCALED_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct rc_channels_scaled_msg {
    uint32_t time_boot_ms;
    int16_t chan1_scaled;
    int16_t chan2_scaled;
    int16_t chan3_scaled;
    int16_t chan4_scaled;
    int16_t chan5_scaled;
    int16_t chan6_scaled;
    int16_t chan7_scaled;
    int16_t chan8_scaled;
    uint8_t port;
    uint8_t rssi;
};
void smavlink_send_rc_channels_scaled(struct rc_channels_scaled_msg* var0,
                                      struct smavlink_out_channel* var1,
                                      struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_RC_CHANNELS_SCALED_H__ */