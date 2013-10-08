#!/bin/bash

HOST=127.0.0.1
PORT=6000
COMMSEC_SERVER=commsec-gcs
DIR=../../../cabal-dev/bin

EXEC_MAVPROXY="python mavlink/mavproxy.py --master=tcp:$HOST:$PORT"

echo starting commsec server: $COMMSEC_SERVER $1
$DIR/$COMMSEC_SERVER $1 &

if [ -z $TERM_APP ]; then
	path_to_gnometerminal = $(which gnome-terminal)
	if [ -x "$path_to_gnometerminal" ]; then
		echo starting mavproxy in a new terminal: $EXEC_MAVPROXY;
		gnome-terminal -x $EXEC_MAVPROXY;
	else
		echo Cannot find a terminal program. Try setting TERM_APP environment variable;
		echo User can start mavproxy with: $EXEC_MAVPROXY;
	fi
else
	echo starting mavproxy in a new terminal: $EXEC_MAVPROXY;
	$TERM_APP -e "$EXEC_MAVPROXY";
fi

function kill() {
    read -p "$*"
}

# Wait for input.
kill "Press [Enter] to kill commsec server..."

# Kill the script.
killall $COMMSEC_SERVER
