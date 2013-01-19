#include <smavlink/pack.h>
#include "smavlink_message_hil_controls.h"
void smavlink_send_hil_controls(struct hil_controls_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2)
{
    uint8_t local0[42U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[42U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->roll_ailerons;
    
    smavlink_pack_float(ref1, 8U, deref3);
    
    float deref4 = *&var0->pitch_elevator;
    
    smavlink_pack_float(ref1, 12U, deref4);
    
    float deref5 = *&var0->yaw_rudder;
    
    smavlink_pack_float(ref1, 16U, deref5);
    
    float deref6 = *&var0->throttle;
    
    smavlink_pack_float(ref1, 20U, deref6);
    
    float deref7 = *&var0->aux1;
    
    smavlink_pack_float(ref1, 24U, deref7);
    
    float deref8 = *&var0->aux2;
    
    smavlink_pack_float(ref1, 28U, deref8);
    
    float deref9 = *&var0->aux3;
    
    smavlink_pack_float(ref1, 32U, deref9);
    
    float deref10 = *&var0->aux4;
    
    smavlink_pack_float(ref1, 36U, deref10);
    
    uint8_t deref11 = *&var0->mode;
    
    smavlink_pack_uint8_t(ref1, 40U, deref11);
    
    uint8_t deref12 = *&var0->nav_mode;
    
    smavlink_pack_uint8_t(ref1, 41U, deref12);
    smavlink_send_ivory(var1, var2, 91U, ref1, 42U, 63U);
    return;
}
