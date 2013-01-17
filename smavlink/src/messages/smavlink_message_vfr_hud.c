#include <smavlink/pack.h>
#include "smavlink_message_vfr_hud.h"
void smavlink_send_vfr_hud(struct vfr_hud_msg* var0,
                           struct smavlink_out_channel* var1,
                           struct smavlink_system* var2)
{
    uint8_t local0[20U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0};
    uint8_t(* ref1)[20U] = &local0;
    float deref2 = *&var0->airspeed;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->groundspeed;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->alt;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->climb;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    int16_t deref6 = *&var0->heading;
    
    smavlink_pack_int16_t(ref1, 16, deref6);
    
    uint16_t deref7 = *&var0->throttle;
    
    smavlink_pack_uint16_t(ref1, 18, deref7);
    smavlink_send_ivory(var1, var2, 74, ref1, 20, 20);
    return;
}
