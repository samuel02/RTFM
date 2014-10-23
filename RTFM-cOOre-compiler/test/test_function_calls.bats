#!/usr/bin/env bats

load test_helper

@test "Calling function inside same class without dot notation should pass" {
  compile function_calls/call_inside_same_class
  assert_success
  assert_last_line "Passed type checking."
}

@test "Calling function in class instance with dot notation should pass" {
  compile function_calls/dotted_call_to_instance
  assert_success
  assert_last_line "Passed type checking."
}