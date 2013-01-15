
#include "smavlink/channel.h"

bool smavlink_channel_begin_atomic( struct smavlink_out_channel *ch,
        size_t len )
{
    void* delegate = ch->write_delegate;
    return (ch->begin_atomic)(delegate, len);
}

void smavlink_channel_end_atomic( struct smavlink_out_channel *ch )
{
    void* delegate = ch->write_delegate;
    return (ch->end_atomic)(delegate);
}

size_t smavlink_channel_write( struct smavlink_out_channel *ch,
        const uint8_t *data, size_t len )
{
    void* delegate = ch->write_delegate;
    return (ch->write)(delegate, data, len);
}

