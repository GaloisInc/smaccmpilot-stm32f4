
#include "termios_helpers.h"
#include <termios.h>

void termios_helper_setraw(int fd, int baud) {
	struct termios t;
	cfmakeraw(&t);
	cfsetispeed(&t, baud);
	cfsetospeed(&t, baud);
	tcsetattr(fd,TCSANOW,&t);
}
