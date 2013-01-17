#ifndef __SMAVLINK_MESSAGE_MEMORY_VECT_H__
#define __SMAVLINK_MESSAGE_MEMORY_VECT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct memory_vect_msg {
    uint16_t address;
    uint8_t ver;
    uint8_t memory_vect_type;
    int8_t value[32U];
};
void smavlink_send_memory_vect(struct memory_vect_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MEMORY_VECT_H__ */