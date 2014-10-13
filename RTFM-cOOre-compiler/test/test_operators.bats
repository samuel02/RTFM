#!/usr/bin/env bats

load test_helper

@test "perform addition correctly" {
  compile_inline "int a := 2 + 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "addition with chars should fail" {
  compile_inline "char a := 'a' + 'c';"
  assert_success
  assert_last_line "TypeError: + operator is not defined for type char."
}

@test "addition with bools should fail" {
  compile_inline "bool a := true + true;"
  assert_success
  assert_last_line "TypeError: + operator is not defined for type bool."
}

@test "addition with strings should fail" {
  compile_inline 'string str := "foo" + "bar";'
  assert_success
  assert_last_line "TypeError: + operator is not defined for type string."
}

@test "perform subtraction correctly" {
  compile_inline "int a := 2 - 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "subtraction with chars should fail" {
  compile_inline "char a := 'a' - 'c';"
  assert_success
  assert_last_line "TypeError: - operator is not defined for type char."
}

@test "subtraction with bools should fail" {
  compile_inline "bool a := true - true;"
  assert_success
  assert_last_line "TypeError: - operator is not defined for type bool."
}

@test "subtraction with strings should fail" {
  compile_inline 'string str := "foo" - "bar";'
  assert_success
  assert_last_line "TypeError: - operator is not defined for type string."
}

@test "perform multiplication correctly" {
  compile_inline "int a := 2 * 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "multiplication with chars should fail" {
  compile_inline "char a := 'a' * 'c';"
  assert_success
  assert_last_line "TypeError: * operator is not defined for type char."
}

@test "multiplication with bools should fail" {
  compile_inline "bool a := true * true;"
  assert_success
  assert_last_line "TypeError: * operator is not defined for type bool."
}

@test "multiplication with strings should fail" {
  compile_inline 'string str := "foo" * "bar";'
  assert_success
  assert_last_line "TypeError: * operator is not defined for type string."
}

@test "perform division correctly" {
  compile_inline "int a := 10 / 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "division with chars should fail" {
  compile_inline "char a := 'a' / 'c';"
  assert_success
  assert_last_line "TypeError: / operator is not defined for type char."
}

@test "division with bools should fail" {
  compile_inline "bool a := true / true;"
  assert_success
  assert_last_line "TypeError: / operator is not defined for type bool."
}

@test "division with strings should fail" {
  compile_inline 'string str := "foo" / "bar";'
  assert_success
  assert_last_line "TypeError: / operator is not defined for type string."
}

@test "perform modulo correctly" {
  compile_inline "int a := 2 %% 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "modulo with chars should fail" {
  compile_inline "char a := 'a' %% 'c';"
  assert_success
  assert_last_line "TypeError: % operator is not defined for type char."
}

@test "modulo with bools should fail" {
  compile_inline "bool a := true %% true;"
  assert_success
  assert_last_line "TypeError: % operator is not defined for type bool."
}

@test "modulo with strings should fail" {
  compile_inline 'string str := "foo" %% "bar";'
  assert_success
  assert_last_line "TypeError: % operator is not defined for type string."
}