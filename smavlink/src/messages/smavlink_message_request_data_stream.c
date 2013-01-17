#include <smavlink/pack.h>
#include "smavlink_message_request_data_stream.h"
void smavlink_send_request_data_stream(struct request_data_stream_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2)
{
    uint8_t local0[6U] = {0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[6U] = &local0;
    uint16_t deref2 = *&var0->req_message_rate;
    
    smavlink_pack_uint16_t(ref1, 0, deref2);
    
    uint8_t deref3 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 2, deref3);
    
    uint8_t deref4 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 3, deref4);
    
    uint8_t deref5 = *&var0->req_stream_id;
    
    smavlink_pack_uint8_t(ref1, 4, deref5);
    
    uint8_t deref6 = *&var0->start_stop;
    
    smavlink_pack_uint8_t(ref1, 5, deref6);
    smavlink_send_ivory(var1, var2, 66, ref1, 6, 148);
    return;
}
