#include <smavlink/pack.h>
#include "smavlink_message_vision_speed_estimate.h"
void smavlink_send_vision_speed_estimate(struct vision_speed_estimate_msg* var0,
                                         struct smavlink_out_channel* var1,
                                         struct smavlink_system* var2)
{
    uint8_t local0[20U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0};
    uint8_t(* ref1)[20U] = &local0;
    uint64_t deref2 = *&var0->usec;
    
    smavlink_pack_uint64_t(ref1, 0, deref2);
    
    float deref3 = *&var0->x;
    
    smavlink_pack_float(ref1, 8, deref3);
    
    float deref4 = *&var0->y;
    
    smavlink_pack_float(ref1, 12, deref4);
    
    float deref5 = *&var0->z;
    
    smavlink_pack_float(ref1, 16, deref5);
    smavlink_send_ivory(var1, var2, 103, ref1, 20, 208);
    return;
}
