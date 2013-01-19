#include <smavlink/pack.h>
#include "smavlink_message_safety_allowed_area.h"
void smavlink_send_safety_allowed_area(struct safety_allowed_area_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2)
{
    uint8_t local0[25U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[25U] = &local0;
    float deref2 = *&var0->p1x;
    
    smavlink_pack_float(ref1, 0U, deref2);
    
    float deref3 = *&var0->p1y;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    float deref4 = *&var0->p1z;
    
    smavlink_pack_float(ref1, 8U, deref4);
    
    float deref5 = *&var0->p2x;
    
    smavlink_pack_float(ref1, 12U, deref5);
    
    float deref6 = *&var0->p2y;
    
    smavlink_pack_float(ref1, 16U, deref6);
    
    float deref7 = *&var0->p2z;
    
    smavlink_pack_float(ref1, 20U, deref7);
    
    uint8_t deref8 = *&var0->frame;
    
    smavlink_pack_uint8_t(ref1, 24U, deref8);
    smavlink_send_ivory(var1, var2, 55U, ref1, 25U, 3U);
    return;
}
