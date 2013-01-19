#include <smavlink/pack.h>
#include "smavlink_message_gps_global_origin.h"
void smavlink_send_gps_global_origin(struct gps_global_origin_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2)
{
    uint8_t local0[12U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[12U] = &local0;
    int32_t deref2 = *&var0->latitude;
    
    smavlink_pack_int32_t(ref1, 0U, deref2);
    
    int32_t deref3 = *&var0->longitude;
    
    smavlink_pack_int32_t(ref1, 4U, deref3);
    
    int32_t deref4 = *&var0->altitude;
    
    smavlink_pack_int32_t(ref1, 8U, deref4);
    smavlink_send_ivory(var1, var2, 49U, ref1, 12U, 39U);
    return;
}
