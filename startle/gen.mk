# subset of Makefile used to generate headers on demand

-include $(OBJS:.o=.d)

SHELL := bash

# collects NAME(...) macros into a sorted list of NAME_ITEM(...) in .gen/name_list.h
.gen/%_list.h.new: NAME=$(shell echo $(notdir $*) | tr a-z A-Z)
.gen/%_list.h.new: $(SRC)
	@mkdir -p $(dir $@)
	sed -n -e 's/^ *'"$(NAME)"'(\(.*\)).*/'"$(NAME)_ITEM"'(\1)/p' $(SRC) | LC_ALL=C sort > $@

# store the current git commit
.gen/git_log.h.new: LOG = $(shell git log -1 --oneline)
.gen/git_log.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@if git diff-index --quiet HEAD --; then \
		echo "#define GIT_LOG \"$(LOG)\"" > $@; \
	else \
		echo "#define GIT_LOG \"$(LOG) [DIRTY]\"" > $@; \
	fi

.gen/%.h.new: %.c startle/bin/makeheaders
	@mkdir -p $(dir $@)
	startle/bin/makeheaders $<:$@

.gen/%.h: .gen/%.h.new $(wildcard %.c)
	@cmp --silent $< $@ || cp $< $@

startle/bin/makeheaders: startle/makeheaders/makeheaders.c
	@mkdir -p startle/bin
	$(CC) -O -w startle/makeheaders/makeheaders.c -o startle/bin/makeheaders
