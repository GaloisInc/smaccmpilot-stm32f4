#!/bin/sh

HOST=127.0.0.1
PORT=6000
COMMSEC_SERVER=../../../cabal-dev/bin/commsec-gcs

EXEC_MAVPROXY="python mavlink/mavproxy.py --master=tcp:$HOST:$PORT --baud=57600"

echo $COMMSEC_SERVER
$COMMSEC_SERVER&

echo $EXEC_MAVPROXY
gnome-terminal -x $EXEC_MAVPROXY
