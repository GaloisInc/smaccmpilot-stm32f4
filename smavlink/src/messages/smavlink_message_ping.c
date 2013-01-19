#include <smavlink/pack.h>
#include "smavlink_message_ping.h"
void smavlink_send_ping(struct ping_msg* var0,
                        struct smavlink_out_channel* var1,
                        struct smavlink_system* var2)
{
    uint8_t local0[14U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[14U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    uint32_t deref3 = *&var0->ping_seq;
    
    smavlink_pack_uint32_t(ref1, 8U, deref3);
    
    uint8_t deref4 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 12U, deref4);
    
    uint8_t deref5 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 13U, deref5);
    smavlink_send_ivory(var1, var2, 4U, ref1, 14U, 237U);
    return;
}
