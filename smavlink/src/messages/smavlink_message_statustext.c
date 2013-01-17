#include <smavlink/pack.h>
#include "smavlink_message_statustext.h"
void smavlink_send_statustext(struct statustext_msg* var0,
                              struct smavlink_out_channel* var1,
                              struct smavlink_system* var2)
{
    uint8_t local0[51U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[51U] = &local0;
    uint8_t deref2 = *&var0->severity;
    
    smavlink_pack_uint8_t(ref1, 0, deref2);
    
    uint8_t(* let3)[50U] = &var0->text;
    uint8_t ix4 = 0 % 50;
    
    for (; ix4 < 49 % 50; ix4 = ix4 + 1 % 50) {
        uint8_t deref5 = *&*let3[ix4];
        
        smavlink_pack_uint8_t(ref1, 1 + ix4, deref5);
    }
    smavlink_send_ivory(var1, var2, 253, ref1, 51, 83);
    return;
}
