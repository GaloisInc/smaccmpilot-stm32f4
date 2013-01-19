#include <smavlink/pack.h>
#include "smavlink_message_param_request_list.h"
void smavlink_send_param_request_list(struct param_request_list_msg* var0,
                                      struct smavlink_out_channel* var1,
                                      struct smavlink_system* var2)
{
    uint8_t local0[2U] = {0, 0};
    uint8_t(* ref1)[2U] = &local0;
    uint8_t deref2 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 1U, deref3);
    smavlink_send_ivory(var1, var2, 21U, ref1, 2U, 159U);
    return;
}
