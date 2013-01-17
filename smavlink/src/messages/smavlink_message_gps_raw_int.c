#include <smavlink/pack.h>
#include "smavlink_message_gps_raw_int.h"
void smavlink_send_gps_raw_int(struct gps_raw_int_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2)
{
    uint8_t local0[30U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[30U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0, deref2);
    
    int32_t deref3 = *&var0->lat;
    
    smavlink_pack_int32_t(ref1, 8, deref3);
    
    int32_t deref4 = *&var0->lon;
    
    smavlink_pack_int32_t(ref1, 12, deref4);
    
    int32_t deref5 = *&var0->alt;
    
    smavlink_pack_int32_t(ref1, 16, deref5);
    
    uint16_t deref6 = *&var0->eph;
    
    smavlink_pack_uint16_t(ref1, 20, deref6);
    
    uint16_t deref7 = *&var0->epv;
    
    smavlink_pack_uint16_t(ref1, 22, deref7);
    
    uint16_t deref8 = *&var0->vel;
    
    smavlink_pack_uint16_t(ref1, 24, deref8);
    
    uint16_t deref9 = *&var0->cog;
    
    smavlink_pack_uint16_t(ref1, 26, deref9);
    
    uint8_t deref10 = *&var0->fix_type;
    
    smavlink_pack_uint8_t(ref1, 28, deref10);
    
    uint8_t deref11 = *&var0->satellites_visible;
    
    smavlink_pack_uint8_t(ref1, 29, deref11);
    smavlink_send_ivory(var1, var2, 24, ref1, 30, 24);
    return;
}
