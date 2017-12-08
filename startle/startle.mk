include startle/gen.mk

print-%:
	@echo $* = $($*)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# pull in dependency info for *existing* .o files
-include $(DEPS)

# generate dependency info and headers
$(BUILD_DIR)/%.d: %.c
	@mkdir -p $(dir $@)
	@$(CC) $(INCLUDE) -MM $(CFLAGS) $*.c -MG -MP -MT $(BUILD_DIR)/$*.o -o- | \
	  sed -E -e 's/ ([a-zA-Z][^ .]*)\.h/ gen\/\1.h/g' > $(BUILD_DIR)/$*.d

# compile
$(BUILD_DIR)/%.o: %.c $(BUILD_DIR)/%.d
	@echo $*.o
	$(CC) -c $(CFLAGS) $*.c -o $(BUILD_DIR)/$*.o

.SECONDARY: $(GEN)
