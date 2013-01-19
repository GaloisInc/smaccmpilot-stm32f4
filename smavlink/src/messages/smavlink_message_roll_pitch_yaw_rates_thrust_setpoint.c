#include <smavlink/pack.h>
#include "smavlink_message_roll_pitch_yaw_rates_thrust_setpoint.h"
void smavlink_send_roll_pitch_yaw_rates_thrust_setpoint(struct roll_pitch_yaw_rates_thrust_setpoint_msg* var0,
                                                        struct smavlink_out_channel* var1,
                                                        struct smavlink_system* var2)
{
    uint8_t local0[20U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0};
    uint8_t(* ref1)[20U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->roll_rate;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    float deref4 = *&var0->pitch_rate;
    
    smavlink_pack_float(ref1, 8U, deref4);
    
    float deref5 = *&var0->yaw_rate;
    
    smavlink_pack_float(ref1, 12U, deref5);
    
    float deref6 = *&var0->thrust;
    
    smavlink_pack_float(ref1, 16U, deref6);
    smavlink_send_ivory(var1, var2, 80U, ref1, 20U, 127U);
    return;
}
