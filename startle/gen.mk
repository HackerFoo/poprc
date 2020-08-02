# subset of Makefile used to generate headers on demand

-include $(OBJS:.o=.d)

SHELL := bash
COMMA := ,

# collects NAME(...) macros into a sorted list of NAME__ITEM(...) in .gen/name_list.h
# NAME can optionally be followed by more uppercase letters: NAME_MORE_WORDS(...)
# which adds NAME_MORE_WORDS__ITEM(...) to the list
# The list will be sorted on everything after the first '('
.gen/%_list.h.new: NAME=$(shell echo $(notdir $*) | tr a-z A-Z)
.gen/%_list.h.new: $(SRC)
	@mkdir -p $(dir $@)
	grep -n '$(NAME)' $(SRC) | \
	  sed -e 'h;s/\(.*\):.*/\1/;y/\//_/;G;s/\n.*:/:/' | \
	  sed -E -n -e 's/^(.*)\.c:(.*): *'"$(NAME)"'(_[A-Z][A-Z_]*)?\((.*)\).*/'"$(NAME)\3__ITEM"'(\1, \2, \4)/p' | \
	  LC_ALL=C sort -t ',' -u -k 3,3 > $@

# store the current git commit
.gen/git_log.h.new: $(SRC)
	@mkdir -p $(dir $@)
	echo -n '#define GIT_LOG "' > $@
	git log -1 --oneline | tr -d '\n' | sed 's/"/\\"/g' >> $@
	@if git diff-index --quiet HEAD --; then \
		echo '"' >> $@; \
	else \
		echo ' [DIRTY]"' >> $@; \
	fi

.PHONY: .gen/file_ids.h.new
.gen/file_ids.h.new: SORTED_SRC := $(sort $(SRC))
.gen/file_ids.h.new: QUOTED_SRC := $(foreach src, $(SORTED_SRC), \"$(src)\")
.gen/file_ids.h.new:
	@mkdir -p $(dir $@)
	rm -f $@
	for i in $(join $(addsuffix $(COMMA), $(subst /,_, $(basename $(SORTED_SRC)))), $(QUOTED_SRC)); do \
		echo "FILE_ID($$i)" >> $@; \
	done

.gen/%-local.h.new: %.c startle/bin/makeheaders
	@mkdir -p $(dir $@)
	startle/bin/makeheaders -local $<:$@

.gen/%.h.new: %.c startle/bin/makeheaders
	@mkdir -p $(dir $@)
	startle/bin/makeheaders $<:$@

.gen/%.h: .gen/%.h.new $(SRC)
	@cmp -s $< $@ || cp $< $@

startle/bin/makeheaders: startle/makeheaders/makeheaders.c
	@mkdir -p startle/bin
	$(CC) -O -w startle/makeheaders/makeheaders.c -o startle/bin/makeheaders
