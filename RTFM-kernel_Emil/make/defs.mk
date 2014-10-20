result = ${shell echo "test"}
ifeq (${result}, test)
	quote = "
else
	quote =
endif

# Build tools
GCC     = arm-none-eabi-gcc
SIZE    = arm-none-eabi-size
OBJDUMP = arm-none-eabi-objdump
OBJCOPY = arm-none-eabi-objcopy
GDB     = arm-none-eabi-gdb
NM      = arm-none-eabi-nm

# Flags
MCU       = -mcpu=cortex-m4 -mthumb -g -mfpu=fpv4-sp-d16 -mfloat-abi=hard 
CFLAGS    = $(MCU) $(COMMON) -std=gnu99 -fno-builtin-exit -O$(OPTIMIZATION) $(INCLUDE) -ffast-math -fsingle-precision-constant
AFLAGS    = $(MCU) $(COMMON) $(INCLUDE) 
LDFLAGS   = $(MCU) $(COMMON) -Tstm32f4x_flash.ld -Wl,--build-id=none,-Map=$(ELFDIR)/$(TARGET).map
BINPLACE  = -j.isr_vector -j.sw_version -j.text -j.ARM.extab -j.ARM 
BINPLACE += -j.preinit_array -j.init_array -j.fini_array -j.data
AFLAGS   += -MD -MP -MF $(DEPDIR)/$(@F).d
CFLAGS   += -MD -MP -MF $(DEPDIR)/$(@F).d

MSG_BINARY_HEX       = ${quote} BIN/HEX  ${quote}
MSG_DUMP             = ${quote} DUMP     ${quote}
MSG_SIZE             = ${quote} SIZE     ${quote}
MSG_LINKING          = ${quote} LD       ${quote}
MSG_COMPILING        = ${quote} CC       ${quote}
MSG_ASSEMBLING       = ${quote} AS       ${quote}
MSG_CLEANING         = ${quote} CLEAN    ${quote}
MSG_EXTENDED_LISTING = ${quote} LIS      ${quote}
MSG_SYMBOL_TABLE     = ${quote} NM       ${quote}

toprel = $(subst $(realpath $(TOP))/,,$(abspath $(1)))

%.hex: %.elf Makefile
	@echo $(MSG_BINARY_HEX) $@
	$(V0) $(OBJCOPY) -O ihex $< $@

%.bin: %.elf Makefile
	@echo $(MSG_BINARY_HEX) $@
	$(V0) $(OBJCOPY) $(BINPLACE) -O binary $< $@
	
%.lss: %.elf Makefile
	@echo $(MSG_EXTENDED_LISTING) $@
	$(V0) $(OBJDUMP) -h -S -C -r $< > $@

%.sym: %.elf Makefile
	@echo $(MSG_SYMBOL_TABLE) $@
	$(V0) $(NM) -n $< > $@

# Compile: Create object files from ASM source files.
define ASSEMBLE_TEMPLATE
$(OBJDIR)/$(notdir $(basename $(1))).o : $(1) Makefile
	@echo $(MSG_ASSEMBLING) $$<
	$(V0) $(GCC) -c $$(AFLAGS) $$< -o $$@
endef

# Compile: Create object files from C source files.
define COMPILE_C_TEMPLATE
$(OBJDIR)/$(notdir $(basename $(1))).o : $(1) Makefile
	@echo $(MSG_COMPILING) $$<
	$(V0) $(GCC) $$(CFLAGS) -c $$< -o $$@
endef

# Link: create ELF output file from object files.
#   $1 = elf file to produce
#   $2 = list of object files that make up the elf file
define LINK_TEMPLATE
.SECONDARY : $(1)
.PRECIOUS : $(2)
$(1):  $(2) Makefile
	@echo $(MSG_LINKING) $$@
	$(V0) $(GCC) $$(CFLAGS) $(2) --output $$@ $$(LDFLAGS) -lm -lc
endef