
#ifndef __SMAVLINK_CHANNEL_H__
#define __SMAVLINK_CHANNEL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

struct smavlink_out_channel {
    uint8_t tx_seq;
    void* write_delegate;
    bool (*begin_atomic)(void*, size_t);
    void (*end_atomic)(void*);
    size_t (*write)(void*, const uint8_t *, size_t);
};

bool smavlink_channel_begin_atomic(struct smavlink_out_channel *ch,
        size_t len);

void smavlink_channel_end_atomic(struct smavlink_out_channel *ch);

size_t smavlink_channel_write(struct smavlink_out_channel *ch,
        const uint8_t *data, size_t len);

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_CHANNEL_H__

