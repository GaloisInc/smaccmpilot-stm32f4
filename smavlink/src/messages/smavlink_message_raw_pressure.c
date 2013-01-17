#include <smavlink/pack.h>
#include "smavlink_message_raw_pressure.h"
void smavlink_send_raw_pressure(struct raw_pressure_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2)
{
    uint8_t local0[16U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[16U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0, deref2);
    
    int16_t deref3 = *&var0->press_abs;
    
    smavlink_pack_int16_t(ref1, 8, deref3);
    
    int16_t deref4 = *&var0->press_diff1;
    
    smavlink_pack_int16_t(ref1, 10, deref4);
    
    int16_t deref5 = *&var0->press_diff2;
    
    smavlink_pack_int16_t(ref1, 12, deref5);
    
    int16_t deref6 = *&var0->temperature;
    
    smavlink_pack_int16_t(ref1, 14, deref6);
    smavlink_send_ivory(var1, var2, 28, ref1, 16, 67);
    return;
}
