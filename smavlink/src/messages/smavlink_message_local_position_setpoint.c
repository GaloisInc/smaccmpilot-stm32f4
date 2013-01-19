#include <smavlink/pack.h>
#include "smavlink_message_local_position_setpoint.h"
void smavlink_send_local_position_setpoint(struct local_position_setpoint_msg* var0,
                                           struct smavlink_out_channel* var1,
                                           struct smavlink_system* var2)
{
    uint8_t local0[17U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[17U] = &local0;
    float deref2 = *&var0->x;
    
    smavlink_pack_float(ref1, 0U, deref2);
    
    float deref3 = *&var0->y;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    float deref4 = *&var0->z;
    
    smavlink_pack_float(ref1, 8U, deref4);
    
    float deref5 = *&var0->yaw;
    
    smavlink_pack_float(ref1, 12U, deref5);
    
    uint8_t deref6 = *&var0->coordinate_frame;
    
    smavlink_pack_uint8_t(ref1, 16U, deref6);
    smavlink_send_ivory(var1, var2, 51U, ref1, 17U, 223U);
    return;
}
