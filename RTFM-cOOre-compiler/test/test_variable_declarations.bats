#!/usr/bin/env bats

load test_helper

@test "declaring integers successfully" {
  compile_inline "int a := 0;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "declaring integers with wrong type" {
  compile_inline "int a := 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot assign char 'a' to int a."
}

@test "declaring bools successfully" {
  compile_inline "bool a := true;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "declaring bools with wrong type" {
  compile_inline "bool a := 3;"
  assert_failure
  assert_last_line "TypeError: Cannot assign int 3 to bool a."
}

@test "declaring chars successfully" {
  compile_inline "char a := 'a';"
  assert_success
  assert_last_line "Passed type checking."
}

@test "declaring chars with wrong type" {
  compile_inline "char a := 2;"
  assert_failure
  assert_last_line "TypeError: Cannot assign int 2 to char a."
}

@test "declaring strings successfully" {
  compile_inline 'string a := "foo";'
  assert_success
  assert_last_line "Passed type checking."
}

@test "declaring strings with wrong type" {
  compile_inline "string a := 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot assign char 'a' to string a."
}

@test "variable must have been declared before use" {
  compile_inline "a := 2;"
  assert_failure
  assert_last_line "NameError: a is not defined."
}