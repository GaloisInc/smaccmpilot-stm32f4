#include <smavlink/pack.h>
#include "smavlink_message_data_stream.h"
void smavlink_send_data_stream(struct data_stream_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[4U] = {0, 0, 0, 0};
    uint8_t(* ref1)[4U] = &local0;
    uint16_t deref2 = *&var0->message_rate;
    
    smavlink_pack_uint16_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->stream_id;
    
    smavlink_pack_uint8_t(ref1, 2U, deref3);
    
    uint8_t deref4 = *&var0->on_off;
    
    smavlink_pack_uint8_t(ref1, 3U, deref4);
    smavlink_send_ivory(var1, var2, 67U, ref1, 4U, 21U);
    return;
}
