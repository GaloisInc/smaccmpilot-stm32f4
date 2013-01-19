#include <smavlink/pack.h>
#include "smavlink_message_servo_output_raw.h"
void smavlink_send_servo_output_raw(struct servo_output_raw_msg* var0,
                                    struct smavlink_out_channel* var1,
                                    struct smavlink_system* var2)
{
    uint8_t local0[21U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0};
    uint8_t(* ref1)[21U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    uint16_t deref3 = *&var0->servo1_raw;
    
    smavlink_pack_uint16_t(ref1, 4U, deref3);
    
    uint16_t deref4 = *&var0->servo2_raw;
    
    smavlink_pack_uint16_t(ref1, 6U, deref4);
    
    uint16_t deref5 = *&var0->servo3_raw;
    
    smavlink_pack_uint16_t(ref1, 8U, deref5);
    
    uint16_t deref6 = *&var0->servo4_raw;
    
    smavlink_pack_uint16_t(ref1, 10U, deref6);
    
    uint16_t deref7 = *&var0->servo5_raw;
    
    smavlink_pack_uint16_t(ref1, 12U, deref7);
    
    uint16_t deref8 = *&var0->servo6_raw;
    
    smavlink_pack_uint16_t(ref1, 14U, deref8);
    
    uint16_t deref9 = *&var0->servo7_raw;
    
    smavlink_pack_uint16_t(ref1, 16U, deref9);
    
    uint16_t deref10 = *&var0->servo8_raw;
    
    smavlink_pack_uint16_t(ref1, 18U, deref10);
    
    uint8_t deref11 = *&var0->port;
    
    smavlink_pack_uint8_t(ref1, 20U, deref11);
    smavlink_send_ivory(var1, var2, 36U, ref1, 21U, 242U);
    return;
}
