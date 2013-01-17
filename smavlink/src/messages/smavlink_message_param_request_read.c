#include <smavlink/pack.h>
#include "smavlink_message_param_request_read.h"
void smavlink_send_param_request_read(struct param_request_read_msg* var0,
                                      struct smavlink_out_channel* var1,
                                      struct smavlink_system* var2)
{
    uint8_t local0[20U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0};
    uint8_t(* ref1)[20U] = &local0;
    int16_t deref2 = *&var0->param_index;
    
    smavlink_pack_int16_t(ref1, 0, deref2);
    
    uint8_t deref3 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 2, deref3);
    
    uint8_t deref4 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 3, deref4);
    
    uint8_t(* let5)[16U] = &var0->param_id;
    uint8_t ix6 = 0 % 16;
    
    for (; ix6 < 15 % 16; ix6 = ix6 + 1 % 16) {
        uint8_t deref7 = *&*let5[ix6];
        
        smavlink_pack_uint8_t(ref1, 4 + ix6, deref7);
    }
    smavlink_send_ivory(var1, var2, 20, ref1, 20, 214);
    return;
}
