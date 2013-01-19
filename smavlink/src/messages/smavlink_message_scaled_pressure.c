#include <smavlink/pack.h>
#include "smavlink_message_scaled_pressure.h"
void smavlink_send_scaled_pressure(struct scaled_pressure_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2)
{
    uint8_t local0[14U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[14U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->press_abs;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    float deref4 = *&var0->press_diff;
    
    smavlink_pack_float(ref1, 8U, deref4);
    
    int16_t deref5 = *&var0->temperature;
    
    smavlink_pack_int16_t(ref1, 12U, deref5);
    smavlink_send_ivory(var1, var2, 29U, ref1, 14U, 115U);
    return;
}
