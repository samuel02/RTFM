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
External bindings are implemented according to [A candidate cOOre module system](https://rtfm.codeplex.com/wikipage?title=Bindings1&referringTitle=2nd%20Challenge).

#### Websockets
Since RTFM is to be used in small embedded systems there is a chance that the system will need to communicate with some other system via internet. For this purpose the "library" `Websocket.core` can be used:

```C
// Example showing the use of websocket send/receive

class Root<> extern "Websocket.core" {
  extern void ws_send(int);

  Task client_receive(char msg) {
    RT_printf("client wrote : %c\n", msg);
  }

  Reset {
    RT_printf("Reset!");
  }

  Idle {
    ws_send(2); // Send number 2 on websocket
    idle_websocket();
  }
}
```

To compile an application using websockets use `PTCORE/RTFM-SRC/wscooremake`.

#### Visualizations
##### Object view
Gives a visual overview of the class declarations and their tasks and functions.
To use, run cOOre compiler flag with flag `-gv_obj obj.gv`.

##### Instance view
Gives a visual overview of the class instances and their tasks and functions.
To use, run cOOre compiler flag with flag `-gv_inst inst.gv`.

##### Task view
Gives a visual overview of the task/function calls between class instances.
To use, run cOOre compiler flag with flag `-gv_task task.gv`.

To generate the pdf files;
`dot -Tpdf task.gv -o task.pdf`

## Installation

### OS X
Tested on OSX Mavericks (10.9) and OSX Yosemite (10.10).

#### Prerequisities

 - XCode (Latest XCode command line tools)

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
1. Download wodi 32bit graphical installer: [http://wodi.forge.ocamlcore.org/download.html](http://wodi.forge.ocamlcore.org/download.html)
2. Install at standard location (C:\wodi32), standard location is important!
3. During Cygwin setup add the following under devel:
    - `binutils`
    - `clang`
    - `gcc-core`
    - `gcc-g++`
    - `gdb`
4. Run "Wodi32 Package Manager", search for menhir and select it for installation
5. Install menhir by clicking on "Apply"

#### Graphviz
- Download graphviz for Windows: [http://www.graphviz.org/Download_windows.php](http://www.graphviz.org/Download_windows.php)
- Run the installer package `graphviz-X.XX.msi`.
- Place it under `C:\GraphvizX.XX\` (**N.B.** NO spaces in the path!)
- Add `C:\GraphvizX.XX\bin` to path.

#### Workaround for Common.ml files
- under Cygwin: run `source fix_common_link.sh`

### Setting up the development environment

#### Make the compilers
```
$ cd RTFM-core-compiler && make rtfm_core
$ cd RTFM-cOOre-compiler && make rtfm_coore
```

## Developing the cOOre compiler

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

or

```bash
$ cd test && bats .
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

## Lost countdown timer
To demonstrate an example application written in cOOre with websockets there is an application written simulating the countdown timer from the TV series [Lost](http://en.wikipedia.org/wiki/Lost_(TV_series)). The specification can be found at [http://lostpedia.wikia.com/wiki/Countdown_timer](http://lostpedia.wikia.com/wiki/Countdown_timer).

In order to test the timer start by compiling the coore program `PTCORE/RTFM-SRC/counter.coore`:

```bash
$ cd PTCORE/RTFM-SRC && ./wscooremake counter.coore
```

Then in order to run the application do

```bash
$ sudo PTCORE/bin/counter
```

To demonstrate the websockets part a web application has been created that uses Javascript and Websockets to connect to the application. The web application can be found under `lost-counter/`. Open `index.html` after you have started the binary and click the button "Connect" in order to connect to the application and start receiving data. If the correct password (4 8 15 16 23 42) is entered the timer will be reset.
