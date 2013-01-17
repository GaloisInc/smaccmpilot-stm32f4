#include <smavlink/pack.h>
#include "smavlink_message_set_local_position_setpoint.h"
void smavlink_send_set_local_position_setpoint(struct set_local_position_setpoint_msg* var0,
                                               struct smavlink_out_channel* var1,
                                               struct smavlink_system* var2)
{
    uint8_t local0[19U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[19U] = &local0;
    float deref2 = *&var0->x;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->y;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->z;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->yaw;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    uint8_t deref6 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 16, deref6);
    
    uint8_t deref7 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 17, deref7);
    
    uint8_t deref8 = *&var0->coordinate_frame;
    
    smavlink_pack_uint8_t(ref1, 18, deref8);
    smavlink_send_ivory(var1, var2, 50, ref1, 19, 214);
    return;
}
