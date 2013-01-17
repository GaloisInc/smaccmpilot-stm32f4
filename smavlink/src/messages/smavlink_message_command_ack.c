#include <smavlink/pack.h>
#include "smavlink_message_command_ack.h"
void smavlink_send_command_ack(struct command_ack_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[3U] = {0, 0, 0};
    uint8_t(* ref1)[3U] = &local0;
    uint16_t deref2 = *&var0->command;
    
    smavlink_pack_uint16_t(ref1, 0, deref2);
    
    uint8_t deref3 = *&var0->result;
    
    smavlink_pack_uint8_t(ref1, 2, deref3);
    smavlink_send_ivory(var1, var2, 77, ref1, 3, 143);
    return;
}
