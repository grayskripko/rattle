######################################################################
# Include git Makefile

GIT_MAKEFILE := $(strip $(wildcard ../git.mk))

ifneq ($(GIT_MAKEFILE),)
    include $(GIT_MAKEFILE)
endif

