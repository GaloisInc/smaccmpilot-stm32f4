
#ifndef __SMAVLINK_RECEIVE_H__
#define __SMAVLINK_RECEIVE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "smavlink/channel.h"
#include "smavlink/system.h"

/**
 * Read a MAVLink message from an input channel.
 *
 * TODO: Should we add a timeout to this function? (And the underlying
 * input channel read?)
 *
 * TODO: Add system and component ID output parameters?  Maybe define
 * a structure containing the non-body output information?
 *
 * @param ch input channel to receive from
 * @param sys MAVLink system information
 * @param msgid_out ID of message on successful receive
 * @param buf buffer to hold packed input message
 * @param len length of "buf"
 * @returns true if a message was successfully received
 */
bool smavlink_receive(struct smavlink_in_channel *ch,
                      struct smavlink_system *sys,
                      uint8_t *msgid_out,
                      uint8_t *buf, size_t len);

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_RECEIVE_H__
