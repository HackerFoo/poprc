# subset of Makefile used to generate headers on demand

-include $(OBJS:.o=.d)

SHELL := bash

gen/word_table.h.new: $(SRC)
	@mkdir -p $(dir $@)
	@sed -n -e 's/.*WORD([^,]*, *\([a-zA-Z0-9_]*\).*).*/extern reduce_t func_\1;/p' $(SRC) | sort > $@
	@echo '#define WORDS {\' >> $@
	@N=$$(cat $(SRC) | grep -e 'WORD(.*)' -c); echo "WORD_COUNT($$N),\\" >> $@
	@sed -n -e 's/.*\(WORD(.*)\).*/\1,\\/p' $(SRC) | LC_ALL=C sort -t \" -k 2,2 >> $@
	@echo '}' >> $@

linenoise/linenoise.h:
	git submodule init
	git submodule update
