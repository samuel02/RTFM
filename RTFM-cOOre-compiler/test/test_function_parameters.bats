#!/usr/bin/env bats

load test_helper

@test "using correct integer parameter" {

  compile function_parameters/correct_int_param
  assert_success
  assert_last_line "Passed type checking."
}

@test "using incorrect integer parameter" {

  compile function_parameters/incorrect_int_param
  assert_failure
  assert_last_line_begins "TypeError"
}

@test "using correct char parameter" {

  compile function_parameters/correct_char_param
  assert_success
  assert_last_line "Passed type checking."
}

@test "using incorrect char parameter" {

  compile function_parameters/incorrect_char_param
  assert_failure
  assert_last_line_begins "TypeError"
}

@test "using correct bool parameter" {

  compile function_parameters/correct_bool_param
  assert_success
  assert_last_line "Passed type checking."
}

@test "using incorrect bool parameter" {

  compile function_parameters/incorrect_bool_param
  assert_failure
  assert_last_line_begins "TypeError"
}

@test "using correct string parameter" {

  compile function_parameters/correct_string_param
  assert_success
  assert_last_line "Passed type checking."
}

@test "using incorrect string parameter" {

  compile function_parameters/incorrect_string_param
  assert_failure
  assert_last_line_begins "TypeError"
}