#include <smavlink/pack.h>
#include "smavlink_message_set_gps_global_origin.h"
void smavlink_send_set_gps_global_origin(struct set_gps_global_origin_msg* var0,
                                         struct smavlink_out_channel* var1,
                                         struct smavlink_system* var2)
{
    uint8_t local0[13U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[13U] = &local0;
    int32_t deref2 = *&var0->latitude;
    
    smavlink_pack_int32_t(ref1, 0, deref2);
    
    int32_t deref3 = *&var0->longitude;
    
    smavlink_pack_int32_t(ref1, 4, deref3);
    
    int32_t deref4 = *&var0->altitude;
    
    smavlink_pack_int32_t(ref1, 8, deref4);
    
    uint8_t deref5 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 12, deref5);
    smavlink_send_ivory(var1, var2, 48, ref1, 13, 41);
    return;
}
