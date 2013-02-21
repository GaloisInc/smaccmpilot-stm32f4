#! /bin/sh

INCS="                                                   \
  -I./smaccmpilot/include/smaccmpilot										 \
  -I./smaccmpilot/include																 \
  -I./hwf4/include																			 \
  -I./include																						 \
  -I./smavlink/include																	 \
  -I./smavlink/include/smavlink/messages								 \
  -I./include"

SRCS=pid_stabilize
pid_stabilize="stabilize_from_rate stabilize_from_angle"

for SRC in $SRCS
do
    for FUNC in ${!SRC}
do
        echo "*** Verifying function $FUNC in source $SRC.c ..."
        cbmc -D IVORY_CBMC $INCS --function $FUNC  smaccmpilot/src/$SRC.c
    done
done
