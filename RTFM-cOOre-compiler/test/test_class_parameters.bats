#!/usr/bin/env bats

load test_helper

@test "instantiation of class with correct parameter types should pass" {
  compile class_parameters/instantiate_with_correct_params
  assert_success
  assert_last_line "Passed type checking."
}

@test "instantiation of class with incorrect parameter types should fail" {
  compile class_parameters/instantiate_with_incorrect_params
  assert_failure
  assert_last_line_begins "TypeError"
}

@test "instantiation of class with too many parameters should fail" {
  compile class_parameters/instantiate_with_too_many_params
  assert_failure
  assert_last_line_begins "TypeError"
}

@test "instantiation of class with too few parameters should fail" {
  compile class_parameters/instantiate_with_too_few_params
  assert_failure
  assert_last_line_begins "TypeError"
}