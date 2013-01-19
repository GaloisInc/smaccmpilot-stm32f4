#include <smavlink/pack.h>
#include "smavlink_message_file_transfer_dir_list.h"
void smavlink_send_file_transfer_dir_list(struct file_transfer_dir_list_msg* var0,
                                          struct smavlink_out_channel* var1,
                                          struct smavlink_system* var2)
{
    uint8_t local0[249U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[249U] = &local0;
    uint64_t deref2 = *&var0->transfer_uid;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->flags;
    
    smavlink_pack_uint8_t(ref1, 248U, deref3);
    
    uint8_t(* let4)[240U] = &var0->dir_path;
    uint8_t ix5 = 0U % 240U;
    
    for (; ix5 < 239U % 240U; ix5 = ix5 + 1U % 240U) {
        uint8_t deref6 = *&*let4[ix5];
        
        smavlink_pack_uint8_t(ref1, 8U + ix5, deref6);
    }
    smavlink_send_ivory(var1, var2, 111U, ref1, 249U, 93U);
    return;
}
