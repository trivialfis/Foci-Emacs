# make -k -C ~/.emacs.d recompile
EMACS ?= emacs
EMACS_DIR := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

EL_DIRS := $(EMACS_DIR) $(EMACS_DIR)/global-settings $(EMACS_DIR)/langs
EL_FILES := $(foreach dir,$(EL_DIRS),$(wildcard $(dir)/*.el))
ELC_FILES := $(EL_FILES:.el=.elc)
LOAD_PATH := $(foreach dir,$(EL_DIRS),-L $(dir))

.PHONY: compile recompile clean

compile: $(ELC_FILES)

recompile:
	@$(MAKE) clean
	@$(MAKE) compile

%.elc: %.el
	@$(EMACS) --batch $(LOAD_PATH) -f batch-byte-compile $<

clean:
	@rm -f $(ELC_FILES)
