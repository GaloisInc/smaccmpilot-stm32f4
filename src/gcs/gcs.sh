#!/bin/sh

HOST=127.0.0.1
PORT=6000
COMMSEC_SERVER=commsec-gcs
DIR=../../../cabal-dev/bin

EXEC_MAVPROXY="python mavlink/mavproxy.py --master=tcp:$HOST:$PORT --baud=57600"

echo $COMMSEC_SERVER
$DIR/$COMMSEC_SERVER&

echo $EXEC_MAVPROXY
gnome-terminal -x $EXEC_MAVPROXY

function kill() {
    read -p "$*"
}

# Wait for input.
kill "Press [Enter] to quit ..."

# Kill the script.
killall $COMMSEC_SERVER
