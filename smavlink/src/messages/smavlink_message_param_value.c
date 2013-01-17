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
    
    smavlink_pack_float(ref1, 0, deref2);
    
    uint16_t deref3 = *&var0->param_count;
    
    smavlink_pack_uint16_t(ref1, 4, deref3);
    
    uint16_t deref4 = *&var0->param_index;
    
    smavlink_pack_uint16_t(ref1, 6, deref4);
    
    uint8_t deref5 = *&var0->param_type;
    
    smavlink_pack_uint8_t(ref1, 24, deref5);
    
    uint8_t(* let6)[16U] = &var0->param_id;
    uint8_t ix7 = 0 % 16;
    
    for (; ix7 < 15 % 16; ix7 = ix7 + 1 % 16) {
        uint8_t deref8 = *&*let6[ix7];
        
        smavlink_pack_uint8_t(ref1, 8 + ix7, deref8);
    }
    smavlink_send_ivory(var1, var2, 22, ref1, 25, 220);
    return;
}
