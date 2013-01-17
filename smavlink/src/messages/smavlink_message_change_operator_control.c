#include <smavlink/pack.h>
#include "smavlink_message_change_operator_control.h"
void smavlink_send_change_operator_control(struct change_operator_control_msg* var0,
                                           struct smavlink_out_channel* var1,
                                           struct smavlink_system* var2)
{
    uint8_t local0[28U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[28U] = &local0;
    uint8_t deref2 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 0, deref2);
    
    uint8_t deref3 = *&var0->control_request;
    
    smavlink_pack_uint8_t(ref1, 1, deref3);
    
    uint8_t deref4 = *&var0->version;
    
    smavlink_pack_uint8_t(ref1, 2, deref4);
    
    uint8_t(* let5)[25U] = &var0->passkey;
    uint8_t ix6 = 0 % 25;
    
    for (; ix6 < 24 % 25; ix6 = ix6 + 1 % 25) {
        uint8_t deref7 = *&*let5[ix6];
        
        smavlink_pack_uint8_t(ref1, 3 + ix6, deref7);
    }
    smavlink_send_ivory(var1, var2, 5, ref1, 28, 217);
    return;
}
