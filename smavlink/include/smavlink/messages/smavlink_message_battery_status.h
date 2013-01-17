#ifndef __SMAVLINK_MESSAGE_BATTERY_STATUS_H__
#define __SMAVLINK_MESSAGE_BATTERY_STATUS_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct battery_status_msg {
    uint16_t voltage_cell_1;
    uint16_t voltage_cell_2;
    uint16_t voltage_cell_3;
    uint16_t voltage_cell_4;
    uint16_t voltage_cell_5;
    uint16_t voltage_cell_6;
    int16_t current_battery;
    uint8_t accu_id;
    int8_t battery_remaining;
};
void smavlink_send_battery_status(struct battery_status_msg* var0,
                                  struct smavlink_out_channel* var1,
                                  struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_BATTERY_STATUS_H__ */