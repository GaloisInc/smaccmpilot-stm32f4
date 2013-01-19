#include <smavlink/pack.h>
#include "smavlink_message_file_transfer_res.h"
void smavlink_send_file_transfer_res(struct file_transfer_res_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2)
{
    uint8_t local0[9U] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[9U] = &local0;
    uint64_t deref2 = *&var0->transfer_uid;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->result;
    
    smavlink_pack_uint8_t(ref1, 8U, deref3);
    smavlink_send_ivory(var1, var2, 112U, ref1, 9U, 124U);
    return;
}
