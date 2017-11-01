# subset of Makefile used to generate headers on demand

-include $(OBJS:.o=.d)

SHELL := bash

gen/test_list.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@sed -n -e 's/^ *int test_\([a-zA-Z0-9_]*\).*/TEST(\1)/p' $(SRC) | LC_ALL=C sort > $@

gen/format_list.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@sed -n -e 's/^ *void format_\([a-zA-Z0-9_]*\).*/FORMAT('"'"'\1'"'"', format_\1)/p' $(SRC) | LC_ALL=C sort > $@

gen/command_list.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@sed -n -e 'N;s/^\/\/ *\(.*\)\n *void command_\([a-zA-Z0-9_]*\).*/COMMAND(\2, "\1")/p;D' $(SRC) | LC_ALL=C sort > $@

gen/git_log.h.new: LOG = $(shell git log -1 --oneline)
gen/git_log.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@if git diff-index --quiet HEAD --; then \
		echo "#define GIT_LOG \"$(LOG)\"" > $@; \
	else \
		echo "#define GIT_LOG \"$(LOG) [DIRTY]\"" > $@; \
	fi

gen/%.h.new: %.c startle/bin/makeheaders
	@mkdir -p $(dir $@)
	startle/bin/makeheaders $<:$@

gen/%.h: gen/%.h.new
	@cmp --silent $< $@ || cp $< $@

startle/bin/makeheaders: startle/makeheaders/makeheaders.c
	@mkdir -p startle/bin
	$(CC) -O -w startle/makeheaders/makeheaders.c -o startle/bin/makeheaders
