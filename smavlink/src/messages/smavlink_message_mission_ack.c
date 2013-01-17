#include <smavlink/pack.h>
#include "smavlink_message_mission_ack.h"
void smavlink_send_mission_ack(struct mission_ack_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[3U] = {0, 0, 0};
    uint8_t(* ref1)[3U] = &local0;
    uint8_t deref2 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 0, deref2);
    
    uint8_t deref3 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 1, deref3);
    
    uint8_t deref4 = *&var0->mission_ack_type;
    
    smavlink_pack_uint8_t(ref1, 2, deref4);
    smavlink_send_ivory(var1, var2, 47, ref1, 3, 153);
    return;
}
