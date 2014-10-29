# RTFM - Realtime For The Masses

## Introduction
RTFM is a set of languages and tools developed to facilitate both concurrent programming in general and embedded real-time software development in particular. There are two languages in RTFM, core and cOOre.

## Getting started
Below is a short introduction to developing applications in the cOOre and the core language.

### cOOre development
cOOre is an object-oriented language based on the -core language. The main goal is to provide a light-weight object-oriented model for concurrent programming.



#### Syntax (EBNF like form)
```
coore:
    classDef*

classDef:
    | "class" ID "<" classArgs ">" extern? "{" classDecl* "}"

classArgs:
    | classArg ("," classArgs)*

classArg:
    | pType ID
    | pType (mSig) ID

mSig:
    | pType ("," mArgs)*

classDecl:
    | pType ID ":=" expr ";"   
    | ID "<" params ">" ID  ";"
    | pType ID "(" mArgs ")" "{" stmt* "}" 
    | "Task" ID "(" mArgs ")" "{" stmt* "}"
    | "Reset" "{" stmt* "}"                                      
    | "Idle" "{" stmt* "}"                                      

mArgs:
    | mArg ("," mArgs)*
  
mArg:
    | pType ID                                                
       
pType:
    | INT | CHAR | BOOL | STRING | VOID

  params:
    | expr ("," params)*                        
      
  expr:                                                        
    | "async" after? before? ids "(" params ")"    (* async expression *)
    | ids LP params RP                             (* id expression    *)
    | ids LP params RP                             (* sync expresion   *)
    | INTVAL                                       (* integer value    *)
    | CHARVAL                                      (* character value  *)
    | BOOLVAL                                      (* boolean value    *)
    | "RT_rand" "(" expr ")"                       (* RT built ins     *)
    | "RT_getc" "(" ")"                                            

  ids:
    | ID "." ID                                                
    | ID                                                       

  after:
    | "after" time                              

  before:
    | "before" time          

  time:
    | INTVAL tunit?

  tuint:
    | "us" | "ms" | "s"   
```

#### External code (core)

#### Websockets

#### Visualizations


### core development

#### Writing core programs

#### Visualizations


## Installation

### OS X
Tested on OSX Mavericks (10.9) and OSX Yosemite (10.10).

#### Prerequisities

 - XCode (Latest XCODE command line tools)

#### Install packages with Homebrew

```
$ brew install ocaml opam graphviz graphviz-gui
```

#### Setup OPAM and install Menhir
```
$ opam init             # The OPAM system needs to be initially configured
$ opam config env       # Run after OPAM init to configure the environment
$ opam install menhir   # Our Parser generator (installs ocamlfind also)
```

### Linux

Tested on Ubuntu 12.04/14.04 and Linux Mint but should work on any modern linux distribution, you may need to change apt-get to your package manager though.

#### Install packages

```
$ sudo add-apt-repository ppa:avsm/ppa    # Add OPAM repository
$ sudo apt-get update                     # Update the repository tree
$ sudo apt-get install python-software-properties ocaml opam m4 graphviz graphviz-gui
```

#### Setup OPAM and install Menhir
```
$ opam init             # The OPAM system needs to be initially configured
$ opam config env       # Run after OPAM init to configure the environment
$ opam install menhir   # Our Parser generator (installs ocamlfind also)
```

### Windows

Tested on Windows 7, 8 and 8.1. All 64-bit. (tested also on win8.1 32-bit)

#### WODI
1. Download wodi 32bit graphical installer: http://wodi.forge.ocamlcore.org/download.html
2. Install at standard location (C:\wodi32), standard location is important!
3. During Cygwin setup add the following under devel:
    - binutils
    - clang
    - gcc-core
    - gcc-g++
    - gdb
4. Run "Wodi32 Package Manager", search for menhir and select it for installation
5. Install menhir by clicking on "Apply"

#### Graphviz
- Download graphviz for Windows: http://www.graphviz.org/Download_windows.php
- Run the installer package "graphviz-X.XX.msi"
- Place it under C:\GraphvizX.XX\, NO spaces in the path!!!
- Add C:\GraphvizX.XX\bin to path.

#### Workaround for Common.ml files
- under Cygwin: run "source fix_common_link.sh"

### Setting up the development environment

#### Make the compilers
```
$ cd RTFM-core-compiler && make rtfm_core
$ cd RTFM-cOOre-compiler && make rtfm_coore
```

## Developing the -core compiler
This should have some text describing how to work with the core compiler, where to find stuff and a basic overview of how it works. Also some information on how debugging works.

## Developing the cOOre compiler
This should have some text describing how to work with the cOOre compiler, where to find stuff and a basic overview of how it works. Also some information on how debugging works.

### Testing
The test suite is using [sstephenson/bats](https://github.com/sstephenson/bats) to automate the testing of the type checker in the cOOre compiler. In order to run the tests, start by installing bats:
 

#### OSX (Homebrew)
```bash
$ brew install bats
```

#### Linux/Windows
See [Installing bats from source](https://github.com/sstephenson/bats#installing-bats-from-source).

### Running the tests
```bash
$ chmod u+x test/test_runner
$ cd test && ./test_runner
```


### Run specific test files
```bash
$ bats test_operators.bats
```

### Writing tests
Tests are written as ```.bats``` files which basically is ```bash``` extended with the light testing framework [sstephenson/bats](https://github.com/sstephenson/bats). The file ```test_helper.bash``` should be loaded in each file since it adds some extra functions to make the tests more slim as well as making sure the compiler is compiled before all tests are run.

#### Example test
```sh
#!/usr/bin/env bats

load test_helper

@test "perform addition correctly" {
  compile_inline "int a := 2 + 3;"
  assert_success
  assert_last_line "Passed type checking."
}

```

#### Helper functions

`compile` takes a filename as argument and will compile the file and then check the result.

`compile_inline` takes a small piece of cOOre code that will be compiled and then checks the result.

`assert_success` asserts that the exit status flag is set to success.

`assert_failure` asserts that the exit status flag is not set to success.

`assert_equal` will check whether the two arguments that are given are euqal and report the difference if they aren't.

`assert_last_line` will check whether the last line of the output is equal to given string.

## History

##### 2014-09-02
Updated Linux install instructions (for Linux Mint etc)

##### 2014-09-01
v1.0 release candidate

##### 2014-07-18
v1.0 development branch
