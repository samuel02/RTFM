#!/usr/bin/env bats

load test_helper

@test "perform addition correctly" {
  compile operators/correct_addition.coore
  assert_success
  assert_last_line "Passed type checking."
}

@test "addition with chars should fail" {
  compile operators/char_addition.coore
  assert_success
  assert_last_line "TypeError: + operator is not defined for type char."
}

@test "addition with bools should fail" {
  compile operators/bool_addition.coore
  assert_success
  assert_last_line "TypeError: + operator is not defined for type bool."
}

@test "addition with strings should fail" {
  compile operators/string_addition.coore
  assert_success
  assert_last_line "TypeError: + operator is not defined for type string."
}