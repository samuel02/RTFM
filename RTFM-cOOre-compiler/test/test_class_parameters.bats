#!/usr/bin/env bats

load test_helper

@test "instantiation of class with correct parameter types should pass" {
  compile class_parameters/instantiate_with_correct_params
  assert_success
  assert_last_line "Passed type checking."
}

@test "instantiation of class with incorrect parameter types should fail" {
  skip "Type checking of class parameters has not been implemented yet."
  compile class_parameters/instantiate_with_incorrect_params
  assert_failure
  assert_last_line "TypeError: Parameter a in class Foo must be of type int."
}