#ifndef __SMAVLINK_MESSAGE_RAW_IMU_H__
#define __SMAVLINK_MESSAGE_RAW_IMU_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct raw_imu_msg {
    uint64_t time_usec;
    int16_t xacc;
    int16_t yacc;
    int16_t zacc;
    int16_t xgyro;
    int16_t ygyro;
    int16_t zgyro;
    int16_t xmag;
    int16_t ymag;
    int16_t zmag;
};
void smavlink_send_raw_imu(struct raw_imu_msg* var0,
                           struct smavlink_out_channel* var1,
                           struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_RAW_IMU_H__ */