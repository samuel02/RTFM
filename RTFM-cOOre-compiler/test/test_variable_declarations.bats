#!/usr/bin/env bats

load test_helper

@test "Declaring integers successfully" {
  compile_inline "int a := 0;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "Declaring integers with wrong type" {
  compile_inline "int a := 'a';"
  assert_failure
  assert_last_line "TypeError: 'a' is not of type int."
}

@test "Declaring bools successfully" {
  compile_inline "bool a := true;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "Declaring bools with wrong type" {
  compile_inline "bool a := 3;"
  assert_failure
  assert_last_line "TypeError: 3 is not of type bool."
}

@test "Declaring chars successfully" {
  compile_inline "char a := 'a';"
  assert_success
  assert_last_line "Passed type checking."
}

@test "Declaring chars with wrong type" {
  compile_inline "char a := 2;"
  assert_failure
  assert_last_line "TypeError: 2 is not of type char."
}

@test "Declaring strings successfully" {
  compile_inline 'string a := "foo";'
  assert_success
  assert_last_line "Passed type checking."
}

@test "Declaring strings with wrong type" {
  compile_inline "string a := 'a';"
  assert_failure
  assert_last_line "TypeError: 'a' is not of type string."
}

@test "Variable must have been declared before use" {
  compile_inline "a := 2;"
  assert_failure
  assert_last_line "NameError: a is not defined."
}