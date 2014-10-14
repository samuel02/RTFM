#!/usr/bin/env bats

load test_helper

@test "instantiation of class with correct parameter types should pass" {
  compile class_parameters/instantiate_with_correct_params
  assert_success
  assert_last_line "Passed type checking."
}