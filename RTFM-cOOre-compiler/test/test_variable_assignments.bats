#!/usr/bin/env bats

load test_helper

@test "assigning integers successfully" {
  compile_inline "int a := 0; a := 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning integers with wrong type" {
  compile_inline "int a := 1; a := 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot assign char 'a' to int a."
}

@test "assigning bools successfully" {
  compile_inline "bool a := true; a := false;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning bools with wrong type" {
  compile_inline "bool a := true; a := 2;"
  assert_failure
  assert_last_line "TypeError: Cannot assign int 2 to bool a."
}

@test "assigning chars successfully" {
  compile_inline "char a := 'a'; a := 'b';"
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning chars with wrong type" {
  compile_inline "char a := 'a'; a := 2;"
  assert_failure
  assert_last_line "TypeError: Cannot assign int 2 to char a."
}

@test "assigning strings successfully" {
  compile_inline 'string a := "foo"; a := "bar";'
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning strings with wrong type" {
  compile_inline 'string a := "foo"; a := 2;'
  assert_failure
  assert_last_line "TypeError: Cannot assign int 2 to string a."
}