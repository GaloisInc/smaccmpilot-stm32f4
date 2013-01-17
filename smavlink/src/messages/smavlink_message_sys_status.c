#include <smavlink/pack.h>
#include "smavlink_message_sys_status.h"
void smavlink_send_sys_status(struct sys_status_msg* var0,
                              struct smavlink_out_channel* var1,
                              struct smavlink_system* var2)
{
    uint8_t local0[31U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[31U] = &local0;
    uint32_t deref2 = *&var0->onboard_control_sensors_present;
    
    smavlink_pack_uint32_t(ref1, 0, deref2);
    
    uint32_t deref3 = *&var0->onboard_control_sensors_enabled;
    
    smavlink_pack_uint32_t(ref1, 4, deref3);
    
    uint32_t deref4 = *&var0->onboard_control_sensors_health;
    
    smavlink_pack_uint32_t(ref1, 8, deref4);
    
    uint16_t deref5 = *&var0->load;
    
    smavlink_pack_uint16_t(ref1, 12, deref5);
    
    uint16_t deref6 = *&var0->voltage_battery;
    
    smavlink_pack_uint16_t(ref1, 14, deref6);
    
    int16_t deref7 = *&var0->current_battery;
    
    smavlink_pack_int16_t(ref1, 16, deref7);
    
    uint16_t deref8 = *&var0->drop_rate_comm;
    
    smavlink_pack_uint16_t(ref1, 18, deref8);
    
    uint16_t deref9 = *&var0->errors_comm;
    
    smavlink_pack_uint16_t(ref1, 20, deref9);
    
    uint16_t deref10 = *&var0->errors_count1;
    
    smavlink_pack_uint16_t(ref1, 22, deref10);
    
    uint16_t deref11 = *&var0->errors_count2;
    
    smavlink_pack_uint16_t(ref1, 24, deref11);
    
    uint16_t deref12 = *&var0->errors_count3;
    
    smavlink_pack_uint16_t(ref1, 26, deref12);
    
    uint16_t deref13 = *&var0->errors_count4;
    
    smavlink_pack_uint16_t(ref1, 28, deref13);
    
    int8_t deref14 = *&var0->battery_remaining;
    
    smavlink_pack_int8_t(ref1, 30, deref14);
    smavlink_send_ivory(var1, var2, 1, ref1, 31, 124);
    return;
}
