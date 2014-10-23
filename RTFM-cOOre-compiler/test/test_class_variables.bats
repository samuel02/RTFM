#!/usr/bin/env bats

load test_helper

@test "assigning bool class variable" {
  compile class_variables/assign_bool_success
  assert_success
  assert_last_line "Passed type checking."
}

@test "assigning bool class variable incorrectly" {

  compile class_variables/assign_bool_incorrect
  assert_failure
  assert_last_line "TypeError: Cannot assign int 2 to bool a."
}

@test "class variables could be used in all class methods" {

  compile class_variables/use_declared_class_variable
  assert_success
  assert_last_line "Passed type checking."
}