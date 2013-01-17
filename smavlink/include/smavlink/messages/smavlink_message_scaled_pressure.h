#ifndef __SMAVLINK_MESSAGE_SCALED_PRESSURE_H__
#define __SMAVLINK_MESSAGE_SCALED_PRESSURE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct scaled_pressure_msg {
    uint32_t time_boot_ms;
    float press_abs;
    float press_diff;
    int16_t temperature;
};
void smavlink_send_scaled_pressure(struct scaled_pressure_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SCALED_PRESSURE_H__ */