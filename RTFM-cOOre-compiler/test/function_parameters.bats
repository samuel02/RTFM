#!/usr/bin/env bats

load test_helper

@test "using correct integer parameter" {
  compile function_parameters/correct_int_param.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "using incorrect integer parameter" {
  compile function_parameters/incorrect_int_param.coore
  assert_success
  assert_last_line "TypeError"
}

@test "using correct char parameter" {
  compile function_parameters/correct_char_param.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "using incorrect char parameter" {
  compile function_parameters/incorrect_char_param.coore
  assert_success
  assert_last_line "TypeError"
}

@test "using correct bool parameter" {
  compile function_parameters/correct_bool_param.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "using incorrect bool parameter" {
  compile function_parameters/incorrect_bool_param.coore
  assert_success
  assert_last_line "TypeError"
}

@test "using correct string parameter" {
  compile function_parameters/correct_string_param.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "using incorrect string parameter" {
  compile function_parameters/incorrect_string_param.coore
  assert_success
  assert_last_line "TypeError"
}