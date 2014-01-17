/**
 * This file should be copied and compiled with the application
 * because the library the number of objects is application specific
 * NOTE: You can override these values to force an upper bound
 *       on the number of objects that the wrapper uses.
 */
#include <rtos-kochab.h>

int _muxid_count = MUTEX_ID_MAX + 1;
int _semid_count = SEM_ID_MAX + 1;
int _taskid_count = TASK_ID_MAX + 1;
