#-------------------------------------------
# Makefile for STM32F4xx targets
# Author: (C) Emil Fresk
#-------------------------------------------

WHEREAMI := $(dir $(lastword $(MAKEFILE_LIST)))
TOP      := $(realpath $(WHEREAMI)/../../../)


REMOVE = rm -f

# Target same	
TARGET ?= rtfm_test

# Verbose
V0 = @
V1 = 

# Where the build will be located and create the folder
OBJDIR = ./build/obj
ELFDIR = ./build

# External high speed crystal frequency
HSE_VALUE = 12000000

# Optimization
OPTIMIZATION = 1
# Create include paths and find sources in all the modules
MODULES = $(wildcard ./modules/*)
$(foreach dir, $(MODULES), $(eval MODULES_INC += -I$(dir)/inc))
$(foreach dir, $(MODULES), $(eval MODULES_CSRCS += $(wildcard $(dir)/src/*.c)))
$(foreach dir, $(MODULES), $(eval MODULES_ASRCS += $(wildcard $(dir)/src/*.s)))

# Sources
INCLUDE = $(MODULES_INC) 
INCLUDE += -I./CMSIS
CSRCS = RTFM-core.c $(wildcard ./CMSIS/*.c) $(MODULES_CSRCS)

# Common
COMMON = -DHSE_VALUE=$(HSE_VALUE) -DSTM32F40_41xxx -fno-builtin-_exit


ASRCS   = $(wildcard ./CMSIS/*.s) $(MODULES_ASRCS) 
ALLSRC = $(ASRCS) $(CSRCS)
ALLSRCBASE = $(notdir $(basename $(ALLSRC)))
ALLOBJECTS = $(addprefix $(OBJDIR)/, $(addsuffix .o, $(ALLSRCBASE)))


# Include defenitions
include make/defs.mk


all: build

build: elf bin hex lss sym size

# Link: Create elf output file from object files.
$(eval $(call LINK_TEMPLATE, $(ELFDIR)/$(TARGET).elf, $(ALLOBJECTS)))

# Assemble: Create object files from assembler source files.
$(foreach src, $(ASRCS), $(eval $(call ASSEMBLE_TEMPLATE, $(src))))

# Compile: Create object files from C source files.
$(foreach src, $(CSRCS), $(eval $(call COMPILE_C_TEMPLATE, $(src))))

elf: $(ELFDIR)/$(TARGET).elf
lss: $(ELFDIR)/$(TARGET).lss
sym: $(ELFDIR)/$(TARGET).sym
hex: $(ELFDIR)/$(TARGET).hex
bin: $(ELFDIR)/$(TARGET).bin

size: $(ELFDIR)/$(TARGET).elf
	$(V0) echo $(MSG_SIZE) $(TARGET).elf
	$(V0) $(SIZE) $(ELFDIR)/$(TARGET).elf	

dump: $(ELFDIR)/$(TARGET).elf
	$(V0) echo $(MSG_DUMP) $(TARGET).elf
	$(V0) $(OBJDUMP) -D $(ELFDIR)/$(TARGET).elf > $(ELFDIR)/$(TARGET)_dump.txt
	

clean: clean_list
	
clean_list:
	$(V0) echo $(MSG_CLEANING)
	$(V0) $(REMOVE) $(ALLOBJECTS)
	$(V0) $(REMOVE) $(ELFDIR)/$(TARGET).map
	$(V0) $(REMOVE) $(ELFDIR)/$(TARGET).elf
	$(V0) $(REMOVE) $(ELFDIR)/$(TARGET).hex
	$(V0) $(REMOVE) $(ELFDIR)/$(TARGET).bin
	$(V0) $(REMOVE) $(ELFDIR)/$(TARGET).sym
	$(V0) $(REMOVE) $(ELFDIR)/$(TARGET).lss
	
