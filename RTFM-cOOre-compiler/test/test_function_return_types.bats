#!/usr/bin/env bats

load test_helper

@test "assigning a function return value to a variable of correct type should pass" {

  compile function_return_types/assign_return_value_correctly
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning a function return value to a variable of wrong type should fail" {

  compile function_return_types/assign_return_value_incorrectly
  assert_failure
  assert_last_line_begins "TypeError"
}

@test "function that returns value of declared type should pass" {

  compile function_return_types/return_declared_type
  assert_success
  assert_last_line "Passed type checking."
}

@test "function that returns value of other type than declared type should fail" {

  compile function_return_types/return_undeclared_type
  assert_success
  assert_last_line "Passed type checking."
}