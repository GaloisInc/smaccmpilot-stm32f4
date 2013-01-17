#include <smavlink/pack.h>
#include "smavlink_message_safety_set_allowed_area.h"
void smavlink_send_safety_set_allowed_area(struct safety_set_allowed_area_msg* var0,
                                           struct smavlink_out_channel* var1,
                                           struct smavlink_system* var2)
{
    uint8_t local0[27U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[27U] = &local0;
    float deref2 = *&var0->p1x;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->p1y;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->p1z;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->p2x;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->p2y;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    float deref7 = *&var0->p2z;
    
    smavlink_pack_float(ref1, 20, deref7);
    
    uint8_t deref8 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 24, deref8);
    
    uint8_t deref9 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 25, deref9);
    
    uint8_t deref10 = *&var0->frame;
    
    smavlink_pack_uint8_t(ref1, 26, deref10);
    smavlink_send_ivory(var1, var2, 54, ref1, 27, 15);
    return;
}
