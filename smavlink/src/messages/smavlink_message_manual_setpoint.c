#include <smavlink/pack.h>
#include "smavlink_message_manual_setpoint.h"
void smavlink_send_manual_setpoint(struct manual_setpoint_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2)
{
    uint8_t local0[22U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0};
    uint8_t(* ref1)[22U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0, deref2);
    
    float deref3 = *&var0->roll;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->pitch;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->yaw;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->thrust;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    uint8_t deref7 = *&var0->mode_switch;
    
    smavlink_pack_uint8_t(ref1, 20, deref7);
    
    uint8_t deref8 = *&var0->manual_override_switch;
    
    smavlink_pack_uint8_t(ref1, 21, deref8);
    smavlink_send_ivory(var1, var2, 81, ref1, 22, 106);
    return;
}
