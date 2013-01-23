
#ifndef __SMAVLINK_CHANNEL_H__
#define __SMAVLINK_CHANNEL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/** An output channel that transmits MAVLink messages. */
struct smavlink_out_channel {
    uint8_t tx_seq;
    void* write_delegate;
    bool (*begin_atomic)(void*, size_t);
    void (*end_atomic)(void*);
    size_t (*write)(void*, const uint8_t *, size_t);
};

/**
 * Enter an atomic section on this output channel.  Other threads
 * attempting to write to this channel will be blocked until the
 * section is ended by calling "smavlink_out_channel_end_atomic".
 */
bool smavlink_out_channel_begin_atomic(struct smavlink_out_channel *ch,
        size_t len);

/** Exit an atomic section on an output channel. */
void smavlink_out_channel_end_atomic(struct smavlink_out_channel *ch);

/**
 * Write "len" bytes from "data" to an output channel "ch".
 *
 * @returns the number of bytes written to the underlying channel
 */
size_t smavlink_out_channel_write(struct smavlink_out_channel *ch,
        const uint8_t *data, size_t len);

/** An input channel that receives MAVLink messages. */
struct smavlink_in_channel {
    void *read_delegate;
    ssize_t (*read)(void *, uint8_t *, size_t);
};

/**
 * Read up to "len" bytes into "data" from an input channel "ch".
 *
 * @returns the number of bytes read into "data", or -1 if a receive
 *          error occurs.  The contents of "data" are undefined if
 *          this returns -1.
 */
ssize_t smavlink_in_channel_read(struct smavlink_in_channel *ch,
                                 uint8_t *data, size_t len);

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_CHANNEL_H__

