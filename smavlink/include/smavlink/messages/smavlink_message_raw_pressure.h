#ifndef __SMAVLINK_MESSAGE_RAW_PRESSURE_H__
#define __SMAVLINK_MESSAGE_RAW_PRESSURE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct raw_pressure_msg {
    uint64_t time_usec;
    int16_t press_abs;
    int16_t press_diff1;
    int16_t press_diff2;
    int16_t temperature;
};
void smavlink_send_raw_pressure(struct raw_pressure_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_RAW_PRESSURE_H__ */