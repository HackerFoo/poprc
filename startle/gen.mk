# subset of Makefile used to generate headers on demand

-include $(OBJS:.o=.d)

SHELL := bash

# collects NAME(...) macros into a sorted list of NAME__ITEM(...) in .gen/name_list.h
# NAME can optionally be followed by more uppercase letters: NAME_MORE_WORDS(...)
# which adds NAME_MORE_WORDS__ITEM(...) to the list
# The list will be sorted on everything after the first '('
.gen/%_list.h.new: NAME=$(shell echo $(notdir $*) | tr a-z A-Z)
.gen/%_list.h.new: $(SRC)
	@mkdir -p $(dir $@)
	sed -E -n -e 's/^ *'"$(NAME)"'(_[A-Z][A-Z_]*)?\((.*)\).*/'"$(NAME)\1__ITEM"'(\2)/p' $(SRC) | LC_ALL=C sort -t '(' -k 2 > $@

# store the current git commit
.gen/git_log.h.new: LOG = $(shell git log -1 --oneline)
.gen/git_log.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@if git diff-index --quiet HEAD --; then \
		echo "#define GIT_LOG \"$(LOG)\"" > $@; \
	else \
		echo "#define GIT_LOG \"$(LOG) [DIRTY]\"" > $@; \
	fi

.gen/%-local.h.new: %.c startle/bin/makeheaders
	@mkdir -p $(dir $@)
	startle/bin/makeheaders -local $<:$@

.gen/%.h.new: %.c startle/bin/makeheaders
	@mkdir -p $(dir $@)
	startle/bin/makeheaders $<:$@

.gen/%.h: .gen/%.h.new $(wildcard %.c)
	@cmp -s $< $@ || cp $< $@

startle/bin/makeheaders: startle/makeheaders/makeheaders.c
	@mkdir -p startle/bin
	$(CC) -O -w startle/makeheaders/makeheaders.c -o startle/bin/makeheaders
