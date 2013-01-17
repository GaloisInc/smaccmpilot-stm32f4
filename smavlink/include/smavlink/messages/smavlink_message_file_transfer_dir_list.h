#ifndef __SMAVLINK_MESSAGE_FILE_TRANSFER_DIR_LIST_H__
#define __SMAVLINK_MESSAGE_FILE_TRANSFER_DIR_LIST_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct file_transfer_dir_list_msg {
    uint64_t transfer_uid;
    uint8_t flags;
    uint8_t dir_path[240U];
};
void smavlink_send_file_transfer_dir_list(struct file_transfer_dir_list_msg* var0,
                                          struct smavlink_out_channel* var1,
                                          struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_FILE_TRANSFER_DIR_LIST_H__ */