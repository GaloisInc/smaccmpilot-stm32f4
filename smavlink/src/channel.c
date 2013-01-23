
#include "smavlink/channel.h"

bool smavlink_out_channel_begin_atomic( struct smavlink_out_channel *ch,
        size_t len )
{
    void* delegate = ch->write_delegate;
    return (ch->begin_atomic)(delegate, len);
}

void smavlink_out_channel_end_atomic( struct smavlink_out_channel *ch )
{
    void* delegate = ch->write_delegate;
    return (ch->end_atomic)(delegate);
}

size_t smavlink_out_channel_write( struct smavlink_out_channel *ch,
        const uint8_t *data, size_t len )
{
    void* delegate = ch->write_delegate;
    return (ch->write)(delegate, data, len);
}


ssize_t smavlink_in_channel_read(struct smavlink_in_channel *ch,
                                 uint8_t *data, size_t len)
{
    return ch->read(ch->read_delegate, data, len);
}
