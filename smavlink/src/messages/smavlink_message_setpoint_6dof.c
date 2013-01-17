#include <smavlink/pack.h>
#include "smavlink_message_setpoint_6dof.h"
void smavlink_send_setpoint_6dof(struct setpoint_6dof_msg* var0,
                                 struct smavlink_out_channel* var1,
                                 struct smavlink_system* var2)
{
    uint8_t local0[25U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[25U] = &local0;
    float deref2 = *&var0->trans_x;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->trans_y;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->trans_z;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->rot_x;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->rot_y;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    float deref7 = *&var0->rot_z;
    
    smavlink_pack_float(ref1, 20, deref7);
    
    uint8_t deref8 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 24, deref8);
    smavlink_send_ivory(var1, var2, 149, ref1, 25, 15);
    return;
}
