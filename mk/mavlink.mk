# Generate new MAVLink message parsers/serializers for the GCS and SMACCMPilot.

MAVLINK_DIR := $(TOP)/../mavlink
SMAVLINK_DIR := src/smaccm-mavlink

MAVLINK_MSG_DEFS := $(MAVLINK_DIR)/message_definitions/v1.0

MAVLINK_DEPS := $(MAVLINK_MSG_DEFS)/smaccmpilot.xml \
                $(MAVLINK_MSG_DEFS)/common.xml

MAVLINK_GEN := $(MAVLINK_DIR)/pymavlink/mavlinkv10.py \

MAVLINK_GCS := $(MAVLINK_DIR)/pymavlink/mavlinkv10.py
MAVLINK_SMACCM := $(wildcard $(SMAVLINK_DIR)/src/SMACCMPilot/Mavlink/Messages/*.hs)

$(MAVLINK_GCS): $(MAVLINK_DEPS)
	@python ./$(MAVLINK_DIR)/pymavlink/generator/mavgen.py \
        --lang=python \
        --wire-protocol=1.0 \
        --output=./$(MAVLINK_DIR)/pymavlink/mavlinkv10.py \
        ./$(MAVLINK_MSG_DEFS)/smaccmpilot.xml

$(MAVLINK_SMACCM): $(MAVLINK_DEPS)
	@python $(SMAVLINK_DIR)/ivory-module-generator/pymavlink/generator/smavgen.py \
    -o ./$(SMAVLINK_DIR)/src/SMACCMPilot/Mavlink/ \
    $(MAVLINK_MSG_DEFS)/smaccmpilot.xml
	@touch $@

MAVLINK := $(MAVLINK_GCS) $(MAVLINK_SMACCM)
