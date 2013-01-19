#include <smavlink/pack.h>
#include "smavlink_message_battery_status.h"
void smavlink_send_battery_status(struct battery_status_msg* var0,
                                  struct smavlink_out_channel* var1,
                                  struct smavlink_system* var2)
{
    uint8_t local0[16U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[16U] = &local0;
    uint16_t deref2 = *&var0->voltage_cell_1;
    
    smavlink_pack_uint16_t(ref1, 0U, deref2);
    
    uint16_t deref3 = *&var0->voltage_cell_2;
    
    smavlink_pack_uint16_t(ref1, 2U, deref3);
    
    uint16_t deref4 = *&var0->voltage_cell_3;
    
    smavlink_pack_uint16_t(ref1, 4U, deref4);
    
    uint16_t deref5 = *&var0->voltage_cell_4;
    
    smavlink_pack_uint16_t(ref1, 6U, deref5);
    
    uint16_t deref6 = *&var0->voltage_cell_5;
    
    smavlink_pack_uint16_t(ref1, 8U, deref6);
    
    uint16_t deref7 = *&var0->voltage_cell_6;
    
    smavlink_pack_uint16_t(ref1, 10U, deref7);
    
    int16_t deref8 = *&var0->current_battery;
    
    smavlink_pack_int16_t(ref1, 12U, deref8);
    
    uint8_t deref9 = *&var0->accu_id;
    
    smavlink_pack_uint8_t(ref1, 14U, deref9);
    
    int8_t deref10 = *&var0->battery_remaining;
    
    smavlink_pack_int8_t(ref1, 15U, deref10);
    smavlink_send_ivory(var1, var2, 147U, ref1, 16U, 42U);
    return;
}
