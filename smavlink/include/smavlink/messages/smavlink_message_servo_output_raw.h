#ifndef __SMAVLINK_MESSAGE_SERVO_OUTPUT_RAW_H__
#define __SMAVLINK_MESSAGE_SERVO_OUTPUT_RAW_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct servo_output_raw_msg {
    uint32_t time_boot_ms;
    uint16_t servo1_raw;
    uint16_t servo2_raw;
    uint16_t servo3_raw;
    uint16_t servo4_raw;
    uint16_t servo5_raw;
    uint16_t servo6_raw;
    uint16_t servo7_raw;
    uint16_t servo8_raw;
    uint8_t port;
};
void smavlink_send_servo_output_raw(struct servo_output_raw_msg* var0,
                                    struct smavlink_out_channel* var1,
                                    struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SERVO_OUTPUT_RAW_H__ */