#include <smavlink/pack.h>
#include "smavlink_message_gps_status.h"
void smavlink_send_gps_status(struct gps_status_msg* var0,
                              struct smavlink_out_channel* var1,
                              struct smavlink_system* var2)
{
    uint8_t local0[101U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[101U] = &local0;
    uint8_t deref2 = *&var0->satellites_visible;
    
    smavlink_pack_uint8_t(ref1, 0U, deref2);
    
    uint8_t(* let3)[20U] = &var0->satellite_prn;
    uint8_t ix4 = 0U % 20U;
    
    for (; ix4 < 19U % 20U; ix4 = ix4 + 1U % 20U) {
        uint8_t deref5 = *&*let3[ix4];
        
        smavlink_pack_uint8_t(ref1, 1U + ix4, deref5);
    }
    
    uint8_t(* let6)[20U] = &var0->satellite_used;
    uint8_t ix7 = 0U % 20U;
    
    for (; ix7 < 19U % 20U; ix7 = ix7 + 1U % 20U) {
        uint8_t deref8 = *&*let6[ix7];
        
        smavlink_pack_uint8_t(ref1, 21U + ix7, deref8);
    }
    
    uint8_t(* let9)[20U] = &var0->satellite_elevation;
    uint8_t ix10 = 0U % 20U;
    
    for (; ix10 < 19U % 20U; ix10 = ix10 + 1U % 20U) {
        uint8_t deref11 = *&*let9[ix10];
        
        smavlink_pack_uint8_t(ref1, 41U + ix10, deref11);
    }
    
    uint8_t(* let12)[20U] = &var0->satellite_azimuth;
    uint8_t ix13 = 0U % 20U;
    
    for (; ix13 < 19U % 20U; ix13 = ix13 + 1U % 20U) {
        uint8_t deref14 = *&*let12[ix13];
        
        smavlink_pack_uint8_t(ref1, 61U + ix13, deref14);
    }
    
    uint8_t(* let15)[20U] = &var0->satellite_snr;
    uint8_t ix16 = 0U % 20U;
    
    for (; ix16 < 19U % 20U; ix16 = ix16 + 1U % 20U) {
        uint8_t deref17 = *&*let15[ix16];
        
        smavlink_pack_uint8_t(ref1, 81U + ix16, deref17);
    }
    smavlink_send_ivory(var1, var2, 25U, ref1, 101U, 23U);
    return;
}
