# RTFM cOOre Compiler
This is a compiler for the RTFM cOOre language, an object-oriented language based on RTFM core.

## Get started
Coming more info soon...

## Type checking
Info about types...

## Building
Info about how to build...

## Tests
The test suite is using [sstephenson/bats](https://github.com/sstephenson/bats) to automate the testing of the type checker in the cOOre compiler. In order to run the tests, start by installing bats:
 

##### OSX (Homebrew)
```bash
$ brew install bats
```

##### Linux/Windows
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
`assert_equal` will check whether the two arguments that are given are euqal and report the difference if they aren't
`assert_last_line` will check whether the last line of the output is equal to given string

## Contributors
Samuel Nilsson, Björn Nilsson, Angelica Brusell
