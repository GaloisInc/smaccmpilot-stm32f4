#include <smavlink/pack.h>
#include "smavlink_message_raw_pressure.h"
void smavlink_send_raw_pressure(struct raw_pressure_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2)
{
    uint8_t local0[16U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[16U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    int16_t deref3 = *&var0->press_abs;
    
    smavlink_pack_int16_t(ref1, 8U, deref3);
    
    int16_t deref4 = *&var0->press_diff1;
    
    smavlink_pack_int16_t(ref1, 10U, deref4);
    
    int16_t deref5 = *&var0->press_diff2;
    
    smavlink_pack_int16_t(ref1, 12U, deref5);
    
    int16_t deref6 = *&var0->temperature;
    
    smavlink_pack_int16_t(ref1, 14U, deref6);
    smavlink_send_ivory(var1, var2, 28U, ref1, 16U, 67U);
    return;
}
