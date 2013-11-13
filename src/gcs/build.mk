include $(TOP)/Keys.mk

GCS_SH    := $(TOP)/gcs.sh
GCS_SH_IN := $(TOP)/src/gcs/gcs.sh.in

$(GCS_SH): $(GCS_SH_IN) $(TOP)/Keys.mk
	@echo "  GEN      $@"
	@sed -e 's/@BASE_ID@/$(BASE_ID)/g'     \
	     -e 's/@BASE_KEY@/$(BASE_KEY)/g'   \
	     -e 's/@BASE_SALT@/$(BASE_SALT)/g' \
	     -e 's/@UAV_KEY@/$(UAV_KEY)/g'     \
	     -e 's/@UAV_SALT@/$(UAV_SALT)/g' < $(GCS_SH_IN) > $(GCS_SH)
	@chmod 755 $(GCS_SH)

OTHER_TARGETS += $(GCS_SH)

