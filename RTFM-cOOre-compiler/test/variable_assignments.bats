#!/usr/bin/env bats

load test_helper

@test "Assigining integers successfully" {
  compile variable_assignments/assign_integers_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Assigining integers with wrong type" {
  compile variable_assignments/assign_integers_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Assigining bools successfully" {
  compile variable_assignments/assign_bools_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Assigining bools with wrong type" {
  compile variable_assignments/assign_bools_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Assigining chars successfully" {
  compile variable_assignments/assign_chars_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Assigining chars with wrong type" {
  compile variable_assignments/assign_chars_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Assigining strings successfully" {
  compile variable_assignments/assign_strings_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Assigining strings with wrong type" {
  compile variable_assignments/assign_strings_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}