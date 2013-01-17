#ifndef __SMAVLINK_MESSAGE_HIL_RC_INPUTS_RAW_H__
#define __SMAVLINK_MESSAGE_HIL_RC_INPUTS_RAW_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct hil_rc_inputs_raw_msg {
    uint64_t time_usec;
    uint16_t chan1_raw;
    uint16_t chan2_raw;
    uint16_t chan3_raw;
    uint16_t chan4_raw;
    uint16_t chan5_raw;
    uint16_t chan6_raw;
    uint16_t chan7_raw;
    uint16_t chan8_raw;
    uint16_t chan9_raw;
    uint16_t chan10_raw;
    uint16_t chan11_raw;
    uint16_t chan12_raw;
    uint8_t rssi;
};
void smavlink_send_hil_rc_inputs_raw(struct hil_rc_inputs_raw_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_HIL_RC_INPUTS_RAW_H__ */