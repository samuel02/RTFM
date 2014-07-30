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
ALL_DOT = task.gv res.gv
ALL_PDF = task.pdf res.pdf

# Flags
OCAMLFLAGS      = -use-menhir
OCAMLCLEANFLAGS = -quiet -clean
DIRFLAGS        = -p
DOTFLAGS        = -gv_task task.gv -gv_res res.gv
PDFFLAGS        = -Tpdf

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

all: rtfm_coore dot

rtfm_coore: application_dir
	$(V0) echo $(quote) Compiling RTFM-core... $(quote)
	$(V0) $(OCAMLBUILD) $(OCAMLFLAGS) $(OCAML_TARGETDIR)$(OCAML_TARGET)

application_dir:
	$(V0) echo $(quote) Creating application directory... $(quote)
	$(V0) $(DIRBUILD) $(DIRFLAGS) $(APPLICATION_TARGETDIR)

dot:
	$(V0) echo $(quote) Generating application and DOT files... $(quote)
	$(V0) $(OCAML_TARGETDIR)$(OCAML_TARGET) $(APPLICATION_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

pdf: pdf_print $(ALL_PDF)

pdf_print:
	$(V0) echo $(quote) Generating PDF files... $(quote)
	 
%.pdf: %.dot
	$(V0) $(DOTBUILD) $(PDFFLAGS) $< -o $@

application:
	$(V0) echo $(quote) Generating application... $(quote)
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(APPLICATION_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS) 

ex1:
	$(V0) echo $(quote) Generating example 1... $(quote)
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE1_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

ex2:
	$(V0) echo $(quote) Generating example 2... $(quote)
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE2_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

ex3:
	$(V0) echo $(quote) Generating example 3... $(quote)
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE3_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

ex4:
	$(V0) echo $(quote) Generating example 4... $(quote)
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE4_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

ex5:
	$(V0) echo $(quote) Generating example 5... $(quote)
	$(OCAML_TARGETDIR)$(OCAML_TARGET) $(EXAMPLE5_SRC) -o $(APPLICATION_TARGETDIR)$(C_TARGET) $(DOTFLAGS)

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
	
