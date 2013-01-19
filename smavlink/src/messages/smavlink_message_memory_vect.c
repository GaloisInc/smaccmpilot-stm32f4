#include <smavlink/pack.h>
#include "smavlink_message_memory_vect.h"
void smavlink_send_memory_vect(struct memory_vect_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[36U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[36U] = &local0;
    uint16_t deref2 = *&var0->address;
    
    smavlink_pack_uint16_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->ver;
    
    smavlink_pack_uint8_t(ref1, 2U, deref3);
    
    uint8_t deref4 = *&var0->memory_vect_type;
    
    smavlink_pack_uint8_t(ref1, 3U, deref4);
    
    int8_t(* let5)[32U] = &var0->value;
    uint8_t ix6 = 0U % 32U;
    
    for (; ix6 < 31U % 32U; ix6 = ix6 + 1U % 32U) {
        int8_t deref7 = *&*let5[ix6];
        
        smavlink_pack_int8_t(ref1, 4U + ix6, deref7);
    }
    smavlink_send_ivory(var1, var2, 249U, ref1, 36U, 204U);
    return;
}
