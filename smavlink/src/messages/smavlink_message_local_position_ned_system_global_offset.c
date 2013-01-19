#include <smavlink/pack.h>
#include "smavlink_message_local_position_ned_system_global_offset.h"
void smavlink_send_local_position_ned_system_global_offset(struct local_position_ned_system_global_offset_msg* var0,
                                                           struct smavlink_out_channel* var1,
                                                           struct smavlink_system* var2)
{
    uint8_t local0[28U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[28U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->x;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    float deref4 = *&var0->y;
    
    smavlink_pack_float(ref1, 8U, deref4);
    
    float deref5 = *&var0->z;
    
    smavlink_pack_float(ref1, 12U, deref5);
    
    float deref6 = *&var0->roll;
    
    smavlink_pack_float(ref1, 16U, deref6);
    
    float deref7 = *&var0->pitch;
    
    smavlink_pack_float(ref1, 20U, deref7);
    
    float deref8 = *&var0->yaw;
    
    smavlink_pack_float(ref1, 24U, deref8);
    smavlink_send_ivory(var1, var2, 89U, ref1, 28U, 231U);
    return;
}
