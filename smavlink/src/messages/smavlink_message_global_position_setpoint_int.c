#include <smavlink/pack.h>
#include "smavlink_message_global_position_setpoint_int.h"
void smavlink_send_global_position_setpoint_int(struct global_position_setpoint_int_msg* var0,
                                                struct smavlink_out_channel* var1,
                                                struct smavlink_system* var2)
{
    uint8_t local0[15U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[15U] = &local0;
    int32_t deref2 = *&var0->latitude;
    
    smavlink_pack_int32_t(ref1, 0U, deref2);
    
    int32_t deref3 = *&var0->longitude;
    
    smavlink_pack_int32_t(ref1, 4U, deref3);
    
    int32_t deref4 = *&var0->altitude;
    
    smavlink_pack_int32_t(ref1, 8U, deref4);
    
    int16_t deref5 = *&var0->yaw;
    
    smavlink_pack_int16_t(ref1, 12U, deref5);
    
    uint8_t deref6 = *&var0->coordinate_frame;
    
    smavlink_pack_uint8_t(ref1, 14U, deref6);
    smavlink_send_ivory(var1, var2, 52U, ref1, 15U, 141U);
    return;
}
