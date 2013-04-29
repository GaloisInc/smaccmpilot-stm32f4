
ifneq ($(CONFIG_EXPORT_SCRATCHPAD),)

SCRATCH = $(CONFIG_EXPORT_SCRATCHPAD)/stm32f4

UPLOADPATH =$(CONFIG_EXPORT_UPLOADPATH)/smaccmpilot-export-$(shell date +'%F').tar.gz

.PHONY: export export-clean
export: all
	mkdir -p $(CONFIG_EXPORT_SCRATCHPAD)
	cp -R $(TOP) $(CONFIG_EXPORT_SCRATCHPAD)/stm32f4
	cp -R $(CONFIG_FREERTOS_PREFIX) $(CONFIG_EXPORT_SCRATCHPAD)/FreeRTOS
	cp -R $(CONFIG_ARDUPILOT_PREFIX) $(CONFIG_EXPORT_SCRATCHPAD)/ardupilot
	rm -rf $(SCRATCH)/.git
	rm -rf $(SCRATCH)/Config.mk
	cp $(SCRATCH)/Config.mk.example $(SCRATCH)/Config.mk
	sed -i "s/..\/OutsideSrcs\///" $(SCRATCH)/Config.mk
	rm -rf $(SCRATCH)/build
	rm -rf $(SCRATCH)/ivory
	rm -rf $(SCRATCH)/tower-test
	rm -rf $(SCRATCH)/sample-rtv-task
	rm -rf $(SCRATCH)/bsp/ivory/ivory-bsp-stm32f4
	rm -rf $(SCRATCH)/bsp/ivory/tests/ledblink
	-rm -rf $(SCRATCH)/mkpdf.sh $(SCRATCH)/out.dot $(SCRATCH)/out.pdf
	-rm -rf $(CONFIG_EXPORT_SCRATCHPAD)/smaccmpilot-export.tar
	-rm -rf $(CONFIG_EXPORT_SCRATCHPAD)/smaccmpilot-export.tar.gz
	cd $(CONFIG_EXPORT_SCRATCHPAD); tar -cf smaccmpilot-export.tar stm32f4 FreeRTOS ardupilot && gzip smaccmpilot-export.tar

export-upload:
	scp $(CONFIG_EXPORT_SCRATCHPAD)/smaccmpilot-export.tar.gz $(UPLOADPATH)

export-clean:
	rm -rf $(CONFIG_EXPORT_SCRATCHPAD)

endif
