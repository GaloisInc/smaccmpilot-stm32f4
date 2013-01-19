#include <smavlink/pack.h>
#include "smavlink_message_change_operator_control_ack.h"
void smavlink_send_change_operator_control_ack(struct change_operator_control_ack_msg* var0,
                                               struct smavlink_out_channel* var1,
                                               struct smavlink_system* var2)
{
    uint8_t local0[3U] = {0, 0, 0};
    uint8_t(* ref1)[3U] = &local0;
    uint8_t deref2 = *&var0->gcs_system_id;
    
    smavlink_pack_uint8_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->control_request;
    
    smavlink_pack_uint8_t(ref1, 1U, deref3);
    
    uint8_t deref4 = *&var0->ack;
    
    smavlink_pack_uint8_t(ref1, 2U, deref4);
    smavlink_send_ivory(var1, var2, 6U, ref1, 3U, 104U);
    return;
}
