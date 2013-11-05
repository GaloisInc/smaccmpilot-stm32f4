#!/bin/bash

HOST=127.0.0.1
PORT=6000
COMMSEC_SERVER=smaccm-gcs-gateway
# COMMSEC_SERVER=commsec-gcs
DIR=../../../.cabal-sandbox/bin

EXEC_MAVPROXY="python mavlink/mavproxy.py --master=tcp:$HOST:$PORT"

# Parameters from "Keys.mk":
SERVER_ARGS="--senderid=5 \
      --sendkey=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] \
      --sendsalt=017235 \
      --recvkey=[15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0] \
      --recvsalt=9219834"

while getopts "dvqs:b:" opt; do
	echo $opt
	case $opt in
		d)
			SERVER_ARGS="$SERVER_ARGS --debug"
			;;
		v)
			SERVER_ARGS="$SERVER_ARGS --verbose"
			;;
		q)
			SERVER_ARGS="$SERVER_ARGS --quiet"
			;;
		s)
			SERVER_ARGS="$SERVER_ARGS --serial=$OPTARG"
			;;
		b)
			SERVER_ARGS="$SERVER_ARGS --baud=$OPTARG"
			;;
		\?)
			echo "Invalid option: -$OPTARG" >&2
			exit 1
			;;
	esac
done

echo starting commsec server: $COMMSEC_SERVER $SERVER_ARGS
$DIR/$COMMSEC_SERVER $SERVER_ARGS &

if [ -z $TERM_APP ]; then
	path_to_gnometerminal=$(which gnome-terminal)
	path_to_xterm=$(which xterm)
	if [ -x "$path_to_gnometerminal" ]; then
		echo starting mavproxy in a new terminal: $EXEC_MAVPROXY;
		gnome-terminal -x $EXEC_MAVPROXY;
  elif [ -x "$path_to_xterm" ]; then
		echo starting mavproxy in a new terminal: $EXEC_MAVPROXY
		xterm -e "$EXEC_MAVPROXY"
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
