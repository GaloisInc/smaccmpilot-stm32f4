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
    
    smavlink_pack_float(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 4U, deref3);
    
    uint8_t deref4 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 5U, deref4);
    
    uint8_t deref5 = *&var0->param_type;
    
    smavlink_pack_uint8_t(ref1, 22U, deref5);
    
    uint8_t(* let6)[16U] = &var0->param_id;
    uint8_t ix7 = 0U % 16U;
    
    for (; ix7 < 15U % 16U; ix7 = ix7 + 1U % 16U) {
        uint8_t deref8 = *&*let6[ix7];
        
        smavlink_pack_uint8_t(ref1, 6U + ix7, deref8);
    }
    smavlink_send_ivory(var1, var2, 23U, ref1, 23U, 168U);
    return;
}
