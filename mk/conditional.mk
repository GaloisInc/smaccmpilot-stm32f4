

# $1 platforms
# $2 proxied call
# $3 proxied arg1
# $4 proxied arg2

define when_platforms
$(if $(findstring $(CONFIG_PLATFORM),$1),$(call $2,$3,$4),)
endef

define unless_platforms
$(if $(findstring $(CONFIG_PLATFORM),$1),,$(call $2,$3,$4))
endef

define when_os
$(if $(findstring $($(CONFIG_PLATFORM)_TOWER_OS),$1),$(call $2,$3,$4,$5),)
endef

define unless_os
$(if $(findstring $($(CONFIG_PLATFORM)_TOWER_OS),$1),,$(call $2,$3,$4,$5))
endef

filteroutstring = $(foreach v,$2,$(if $(findstring $1,$v),,$v))
