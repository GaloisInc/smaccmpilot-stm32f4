#include <smavlink/pack.h>
#include "smavlink_message_set_roll_pitch_yaw_thrust.h"
void smavlink_send_set_roll_pitch_yaw_thrust(struct set_roll_pitch_yaw_thrust_msg* var0,
                                             struct smavlink_out_channel* var1,
                                             struct smavlink_system* var2)
{
    uint8_t local0[18U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[18U] = &local0;
    float deref2 = *&var0->roll;
    
    smavlink_pack_float(ref1, 0U, deref2);
    
    float deref3 = *&var0->pitch;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    float deref4 = *&var0->yaw;
    
    smavlink_pack_float(ref1, 8U, deref4);
    
    float deref5 = *&var0->thrust;
    
    smavlink_pack_float(ref1, 12U, deref5);
    
    uint8_t deref6 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 16U, deref6);
    
    uint8_t deref7 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 17U, deref7);
    smavlink_send_ivory(var1, var2, 56U, ref1, 18U, 100U);
    return;
}
