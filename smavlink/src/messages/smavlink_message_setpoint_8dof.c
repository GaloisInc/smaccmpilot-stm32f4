#include <smavlink/pack.h>
#include "smavlink_message_setpoint_8dof.h"
void smavlink_send_setpoint_8dof(struct setpoint_8dof_msg* var0,
                                 struct smavlink_out_channel* var1,
                                 struct smavlink_system* var2)
{
    uint8_t local0[33U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[33U] = &local0;
    float deref2 = *&var0->val1;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->val2;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->val3;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->val4;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->val5;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    float deref7 = *&var0->val6;
    
    smavlink_pack_float(ref1, 20, deref7);
    
    float deref8 = *&var0->val7;
    
    smavlink_pack_float(ref1, 24, deref8);
    
    float deref9 = *&var0->val8;
    
    smavlink_pack_float(ref1, 28, deref9);
    
    uint8_t deref10 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 32, deref10);
    smavlink_send_ivory(var1, var2, 148, ref1, 33, 241);
    return;
}
