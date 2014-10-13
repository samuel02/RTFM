#!/usr/bin/env bats

load test_helper

@test "Assigning integers successfully" {
  compile variable_assignments/assign_integers_success.coore
  assert_success
  assert_last_line "Passed type checking."
}

@test "Assigning integers with wrong type" {
  compile variable_assignments/assign_integers_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Assigning bools successfully" {
  compile variable_assignments/assign_bools_success.coore
  assert_success
  assert_last_line "Passed type checking."
}

@test "Assigning bools with wrong type" {
  compile variable_assignments/assign_bools_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Assigning chars successfully" {
  compile variable_assignments/assign_chars_success.coore
  assert_success
  assert_last_line "Passed type checking."
}

@test "Assigning chars with wrong type" {
  compile variable_assignments/assign_chars_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Assigning strings successfully" {
  compile variable_assignments/assign_strings_success.coore
  assert_success
  assert_last_line "Passed type checking."
}

@test "Assigning strings with wrong type" {
  compile variable_assignments/assign_strings_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}