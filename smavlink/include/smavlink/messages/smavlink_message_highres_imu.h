#ifndef __SMAVLINK_MESSAGE_HIGHRES_IMU_H__
#define __SMAVLINK_MESSAGE_HIGHRES_IMU_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct highres_imu_msg {
    uint64_t time_usec;
    float xacc;
    float yacc;
    float zacc;
    float xgyro;
    float ygyro;
    float zgyro;
    float xmag;
    float ymag;
    float zmag;
    float abs_pressure;
    float diff_pressure;
    float pressure_alt;
    float temperature;
    uint16_t fields_updated;
};
void smavlink_send_highres_imu(struct highres_imu_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_HIGHRES_IMU_H__ */