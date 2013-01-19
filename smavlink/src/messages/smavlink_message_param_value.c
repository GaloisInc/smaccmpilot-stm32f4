#include <smavlink/pack.h>
#include "smavlink_message_param_value.h"
void smavlink_send_param_value(struct param_value_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[25U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[25U] = &local0;
    float deref2 = *&var0->param_value;
    
    smavlink_pack_float(ref1, 0U, deref2);
    
    uint16_t deref3 = *&var0->param_count;
    
    smavlink_pack_uint16_t(ref1, 4U, deref3);
    
    uint16_t deref4 = *&var0->param_index;
    
    smavlink_pack_uint16_t(ref1, 6U, deref4);
    
    uint8_t deref5 = *&var0->param_type;
    
    smavlink_pack_uint8_t(ref1, 24U, deref5);
    
    uint8_t(* let6)[16U] = &var0->param_id;
    uint8_t ix7 = 0U % 16U;
    
    for (; ix7 < 15U % 16U; ix7 = ix7 + 1U % 16U) {
        uint8_t deref8 = *&*let6[ix7];
        
        smavlink_pack_uint8_t(ref1, 8U + ix7, deref8);
    }
    smavlink_send_ivory(var1, var2, 22U, ref1, 25U, 220U);
    return;
}
