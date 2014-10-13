#!/usr/bin/env bats

load test_helper

@test "perform addition correctly" {
  compile_inline "int a := 2 + 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "addition with chars should fail" {
  compile_inline "char a := 'a' + 'c';"
  assert_success
  assert_last_line "TypeError: + operator is not defined for type char."
}

@test "addition with bools should fail" {
  compile_inline "bool a := true + true;"
  assert_success
  assert_last_line "TypeError: + operator is not defined for type bool."
}

@test "addition with strings should fail" {
  compile_inline 'string str := "foo" + "bar";'
  assert_success
  assert_last_line "TypeError: + operator is not defined for type string."
}