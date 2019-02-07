include startle/gen.mk

print-%:
	@echo $* = $($*)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# pull in dependency info for *existing* .o files
-include $(DEPS)

# generate dependency info and headers
$(BUILD_DIR)/%.d: %.c
	@mkdir -p $(dir $@)
	@$(CC) $(INCLUDE) -MM $(CFLAGS) $*.c -MG -MT $(BUILD_DIR)/$*.o -o- 2>/dev/null | \
	  sed -E -e 's/ ([a-zA-Z][^ .]*)\.h/ .gen\/\1.h/g' > $(BUILD_DIR)/$*.d

LOCAL_HEADERS += $(shell find -L . -not -path './.gen/*' -name '*.h')
LOCAL_HEADERS := $(sort $(LOCAL_HEADERS))
GEN_LOCAL_HEADERS := $(patsubst ./%.h, .gen/%.h, $(LOCAL_HEADERS))

# hack to catch any dependencies in .gen that are local headers
$(GEN_LOCAL_HEADERS): .gen/%.h: %.h
	@mkdir -p $(dir $@)
	ln -fs $(PWD)/$*.h .gen/$*.h

# compile
$(BUILD_DIR)/%.o: %.c $(BUILD_DIR)/%.d
	@echo $*.o
	$(CC) -c $(CFLAGS) -DFILEBASE=$(subst /,_,$*) $*.c -o $(BUILD_DIR)/$*.o

.SECONDARY: $(GEN)
