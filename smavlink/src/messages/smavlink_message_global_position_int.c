#include <smavlink/pack.h>
#include "smavlink_message_global_position_int.h"
void smavlink_send_global_position_int(struct global_position_int_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2)
{
    uint8_t local0[28U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[28U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    int32_t deref3 = *&var0->lat;
    
    smavlink_pack_int32_t(ref1, 4U, deref3);
    
    int32_t deref4 = *&var0->lon;
    
    smavlink_pack_int32_t(ref1, 8U, deref4);
    
    int32_t deref5 = *&var0->alt;
    
    smavlink_pack_int32_t(ref1, 12U, deref5);
    
    int32_t deref6 = *&var0->relative_alt;
    
    smavlink_pack_int32_t(ref1, 16U, deref6);
    
    int16_t deref7 = *&var0->vx;
    
    smavlink_pack_int16_t(ref1, 20U, deref7);
    
    int16_t deref8 = *&var0->vy;
    
    smavlink_pack_int16_t(ref1, 22U, deref8);
    
    int16_t deref9 = *&var0->vz;
    
    smavlink_pack_int16_t(ref1, 24U, deref9);
    
    uint16_t deref10 = *&var0->hdg;
    
    smavlink_pack_uint16_t(ref1, 26U, deref10);
    smavlink_send_ivory(var1, var2, 33U, ref1, 28U, 104U);
    return;
}
