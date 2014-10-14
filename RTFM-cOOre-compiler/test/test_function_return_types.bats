#!/usr/bin/env bats

load test_helper

@test "assigning a function return value to a variable of correct type should pass" {
  skip "Type checking for function return values has not been implemented yet"
  compile function_return_types/assign_return_value_correctly
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning a function return value to a variable of wrong type should fail" {
  skip "Type checking for function return values has not been implemented yet"
  compile function_return_types/assign_return_value_incorrectly
  assert_failure
  assert_last_line "TypeError: Function true() returns a bool but val is of type int."
}

@test "function that returns value of declared type should pass" {
  skip "Type checking for function return values has not been implemented yet"
  compile function_return_types/return_declared_type
  assert_success
  assert_last_line "Passed type checking."
}

@test "function that returns value of other type than declared type should fail" {
  skip "Type checking for function return values has not been implemented yet"
  compile function_return_types/return_undeclared_type
  assert_success
  assert_last_line "Passed type checking."
}