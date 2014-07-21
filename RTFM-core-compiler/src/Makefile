#-------------------------------------------
# Makefile for RTFM -core
# Author: (C) Emil Fresk
#-------------------------------------------

# Quote test
result = ${shell echo "test"}
ifeq (${result}, test)
	quote = "
else
	quote =
endif

# Compilers
OCAMLBUILD = ocamlbuild
DOTBUILD   = dot
DIRBUILD   = mkdir

# Remove
REMOVE     = rm -f
REMOVE_DIR = rm -fr

# Remove files
ALL_DOT = out.dot lout.dot
ALL_PDF = out.pdf lout.pdf

# Flags
OCAMLFLAGS      = -use-menhir
OCAMLCLEANFLAGS = -quiet -clean
DIRFLAGS        = -p
DOTFLAGS        = -d out.dot -ldot lout.dot

# Application directory
APPLICATION_DIR = ../../PTCORE/RTFM-SRC/

# Application and example files
APPLICATION_SRC = $(APPLICATION_DIR)Application.core
EXAMPLE1_SRC = $(APPLICATION_DIR)Ex1.core
EXAMPLE2_SRC = $(APPLICATION_DIR)Ex2.core
EXAMPLE3_SRC = $(APPLICATION_DIR)Ex3.core
EXAMPLE4_SRC = $(APPLICATION_DIR)Ex4.core
EXAMPLE5_SRC = $(APPLICATION_DIR)Ex5.core

# Application target directory
APPLICATION_TARGETDIR = ../../PTCORE/Application/
OCAML_TARGETDIR = ./

# Target file
OCAML_TARGET = Main.native
C_TARGET     = autogen.c

# Verbose
V0 = @
V1 = 

all: rtfm_core dot

rtfm_core: application_dir
	$(OCAMLBUILD) $(OCAMLFLAGS) $(OCAML_TARGETDIR)$(OCAML_TARGET)

application_dir:
	$(DIRBUILD) $(DIRFLAGS) $(APPLICATION_TARGETDIR)

dot:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(APPLICATION_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

application:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(APPLICATION_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET)

ex1:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE1_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET)

ex2:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE2_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET)

ex3:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE3_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET)

ex4:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE4_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET)

ex5:
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE5_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET)

clean: clean_ocaml clean_application clean_dot clean_pdf

clean_dot:
	$(V0) echo $(quote) Removing generated DOT files... $(quote)
	$(V0) $(REMOVE) $(ALL_DOT)

clean_pdf:
	$(V0) echo $(quote) Removing generated PDF files... $(quote)
	$(V0) $(REMOVE) $(ALL_PDF)

clean_ocaml:
	$(V0) echo $(quote) Removing generated OCaml files... $(quote)
	$(V0) $(OCAMLBUILD) $(OCAMLCLEANFLAGS)
	$(V0) $(REMOVE) $(OCAML_TARGETDIR)$(OCAML_TARGET)

clean_application:
	$(V0) echo $(quote) Removing generated application files... $(quote)
	$(V0) $(REMOVE_DIR) $(APPLICATION_TARGETDIR)
	
