# Generate new MAVLink message parsers/serializers for the GCS and SMACCMPilot.

include mk/cmd.lib

MAVLINK_DIR := ./../mavlink
SMACCM_MAVLINK_DIR := ./src/smaccm-mavlink

MAVLINK_MSG_DEFS := $(MAVLINK_DIR)/message_definitions/v1.0

MAVLINK_DEPS := $(MAVLINK_MSG_DEFS)/smaccmpilot.xml \
                $(MAVLINK_MSG_DEFS)/common.xml

MAVLINK_GCS := $(MAVLINK_DIR)/pymavlink/mavlinkv10.py
MAVLINK_SMACCM := $(wildcard $(SMACCM_MAVLINK_DIR)/src/SMACCMPilot/Mavlink/Messages/*.hs)

$(MAVLINK_GCS): $(MAVLINK_DEPS)
	@python ./$(MAVLINK_DIR)/pymavlink/generator/mavgen.py \
        --lang=python \
        --wire-protocol=1.0 \
        --output=./$(MAVLINK_DIR)/pymavlink/mavlinkv10.py \
        ./$(MAVLINK_MSG_DEFS)/smaccmpilot.xml

$(MAVLINK_SMACCM): $(MAVLINK_GCS) $(MAVLINK_DEPS)
	$(Q)python $(SMACCM_MAVLINK_DIR)/ivory-module-generator/pymavlink/generator/smavgen.py \
		-o ./$(SMACCM_MAVLINK_DIR)/src/SMACCMPilot/Mavlink/ \
		$(MAVLINK_MSG_DEFS)/smaccmpilot.xml
	$(Q)touch $@
	$(Q)echo
	$(Q)echo "WARNING: **************************************************************"
	$(Q)echo "WARNING: smaccm-mavlink library regenerated. If you added new messages"
	$(Q)echo "WARNING: to the mavlink message definitions, you will need to add the"
	$(Q)echo "WARNING: generated haskell sources to version control and"
	$(Q)echo "WARNING: src/smaccm-mavlink/smaccm-mavlink.cabal manually!"
	$(Q)echo "WARNING: **************************************************************"
	$(Q)echo

MAVLINK_TARGETS := $(MAVLINK_SMACCM)

