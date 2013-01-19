#include <smavlink/pack.h>
#include "smavlink_message_system_time.h"
void smavlink_send_system_time(struct system_time_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[12U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[12U] = &local0;
    uint64_t deref2 = *&var0->time_unix_usec;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    uint32_t deref3 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 8U, deref3);
    smavlink_send_ivory(var1, var2, 2U, ref1, 12U, 137U);
    return;
}
