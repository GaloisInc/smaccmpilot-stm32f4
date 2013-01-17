#include <smavlink/pack.h>
#include "smavlink_message_param_set.h"
void smavlink_send_param_set(struct param_set_msg* var0,
                             struct smavlink_out_channel* var1,
                             struct smavlink_system* var2)
{
    uint8_t local0[23U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0};
    uint8_t(* ref1)[23U] = &local0;
    float deref2 = *&var0->param_value;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    uint8_t deref3 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 4, deref3);
    
    uint8_t deref4 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 5, deref4);
    
    uint8_t deref5 = *&var0->param_type;
    
    smavlink_pack_uint8_t(ref1, 22, deref5);
    
    uint8_t(* let6)[16U] = &var0->param_id;
    uint8_t ix7 = 0 % 16;
    
    for (; ix7 < 15 % 16; ix7 = ix7 + 1 % 16) {
        uint8_t deref8 = *&*let6[ix7];
        
        smavlink_pack_uint8_t(ref1, 6 + ix7, deref8);
    }
    smavlink_send_ivory(var1, var2, 23, ref1, 23, 168);
    return;
}
