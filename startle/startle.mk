include startle/gen.mk

print-%:
	@echo $* = $($*)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# pull in dependency info for *existing* .o files
-include $(DEPS)

# generate dependency info and headers
$(BUILD_DIR)/%.d: %.c
	@mkdir -p $(dir $@)
	@$(CC) $(INCLUDE) -MM $(CFLAGS) $*.c -MG -MT $(BUILD_DIR)/$*.o -o- | \
	  sed -E -e 's/ ([a-zA-Z][^ .]*)\.h/ .gen\/\1.h/g' > $(BUILD_DIR)/$*.d

LOCAL_HEADERS := $(patsubst ./%.h, .gen/%.h, $(shell find -L . -not -path './.gen/*' -name '*.h'))

# hack to catch any dependencies in .gen that are local headers
$(LOCAL_HEADERS): .gen/%.h:
	@mkdir -p $(dir $@)
	ln -s $(PWD)/$*.h .gen/$*.h

# compile
$(BUILD_DIR)/%.o: %.c $(BUILD_DIR)/%.d
	@echo $*.o
	$(CC) -c $(CFLAGS) $*.c -o $(BUILD_DIR)/$*.o

.SECONDARY: $(GEN)
