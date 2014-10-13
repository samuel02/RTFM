#!/usr/bin/env bats

load test_helper

@test "Declaring integers successfully" {
  compile variable_declarations/declare_integers_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Declaring integers with wrong type" {
  compile variable_declarations/declare_integers_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Declaring bools successfully" {
  compile variable_declarations/declare_bools_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Declaring bools with wrong type" {
  compile variable_declarations/declare_bools_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Declaring chars successfully" {
  compile variable_declarations/declare_chars_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Declaring chars with wrong type" {
  compile variable_declarations/declare_chars_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}

@test "Declaring strings successfully" {
  compile variable_declarations/declare_strings_success.coore
  assert_success
  assert_last_line "Typechecking successful"
}

@test "Declaring strings with wrong type" {
  compile variable_declarations/declare_strings_wrong_type.coore
  assert_success
  assert_last_line "Assignment: a"
}