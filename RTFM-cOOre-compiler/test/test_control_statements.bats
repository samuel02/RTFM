#!/usr/bin/env bats

load test_helper

@test "condition in if-statement can be a bool" {
  compile_inline 'if(true) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in if-statement can be an int" {
  compile_inline 'if(2) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in if-statement can be evaluated to a bool" {
  compile_inline 'if(true == false) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in if-statement can be evaluated to an int" {
  compile_inline 'if(2+2) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in if-statement cannot be a char" {
  compile_inline "if('a') {}"
  assert_failure
  assert_last_line "TypeError: Condition in if-statement must be evaluated to type int or bool."
}

@test "condition in if-statement cannot be a string" {
  compile_inline 'if("foo") {}'
  assert_failure
  assert_last_line "TypeError: Condition in if-statement must be evaluated to type int or bool."
}

@test "condition in while-statement can be a bool" {
  compile_inline 'while(true) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in while-statement can be an int" {
  compile_inline 'while(2) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in while-statement can be evaluated to a bool" {
  compile_inline 'while(true == false) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in while-statement can be evaluated to an int" {
  compile_inline 'while(2+2) {}'
  assert_success
  assert_last_line "Passed type checking."
}

@test "condition in while-statement cannot be a char" {
  compile_inline "while('a') {}"
  assert_failure
  assert_last_line "TypeError: Condition in while-statement must be evaluated to type int or bool."
}

@test "condition in while-statement cannot be a string" {
  compile_inline 'while("foo") {}'
  assert_failure
  assert_last_line "TypeError: Condition in while-statement must be evaluated to type int or bool."
}