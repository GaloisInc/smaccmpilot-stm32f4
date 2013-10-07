include ../../Keys.mk

ARGS = \
  --senderid=$(BASE_ID) \
  --sendkey=$(BASE_KEY) \
  --sendsalt=$(BASE_SALT) \
  --recvkey=$(UAV_KEY) \
  --recvsalt=$(UAV_SALT) \

.PHONY: all
all:
	./gcs.sh "$(ARGS)"
