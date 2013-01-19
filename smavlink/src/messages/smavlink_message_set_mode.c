#include <smavlink/pack.h>
#include "smavlink_message_set_mode.h"
void smavlink_send_set_mode(struct set_mode_msg* var0,
                            struct smavlink_out_channel* var1,
                            struct smavlink_system* var2)
{
    uint8_t local0[6U] = {0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[6U] = &local0;
    uint32_t deref2 = *&var0->custom_mode;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 4U, deref3);
    
    uint8_t deref4 = *&var0->base_mode;
    
    smavlink_pack_uint8_t(ref1, 5U, deref4);
    smavlink_send_ivory(var1, var2, 11U, ref1, 6U, 89U);
    return;
}
