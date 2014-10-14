#!/usr/bin/env bats

load test_helper

@test "perform addition correctly" {
  compile_inline "int a := 2 + 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "addition with chars should fail" {
  compile_inline "char a := 'a' + 'c';"
  assert_failure
  assert_last_line "TypeError: + operator is not defined for type char."
}

@test "addition with bools should fail" {
  compile_inline "bool a := true + true;"
  assert_failure
  assert_last_line "TypeError: + operator is not defined for type bool."
}

@test "addition with strings should fail" {
  compile_inline 'string str := "foo" + "bar";'
  assert_failure
  assert_last_line "TypeError: + operator is not defined for type string."
}

@test "perform subtraction correctly" {
  compile_inline "int a := 2 - 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "subtraction with chars should fail" {
  compile_inline "char a := 'a' - 'c';"
  assert_failure
  assert_last_line "TypeError: - operator is not defined for type char."
}

@test "subtraction with bools should fail" {
  compile_inline "bool a := true - true;"
  assert_failure
  assert_last_line "TypeError: - operator is not defined for type bool."
}

@test "subtraction with strings should fail" {
  compile_inline 'string str := "foo" - "bar";'
  assert_failure
  assert_last_line "TypeError: - operator is not defined for type string."
}

@test "perform multiplication correctly" {
  compile_inline "int a := 2 * 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "multiplication with chars should fail" {
  compile_inline "char a := 'a' * 'c';"
  assert_failure
  assert_last_line "TypeError: * operator is not defined for type char."
}

@test "multiplication with bools should fail" {
  compile_inline "bool a := true * true;"
  assert_failure
  assert_last_line "TypeError: * operator is not defined for type bool."
}

@test "multiplication with strings should fail" {
  compile_inline 'string str := "foo" * "bar";'
  assert_failure
  assert_last_line "TypeError: * operator is not defined for type string."
}

@test "perform division correctly" {
  compile_inline "int a := 10 / 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "division with chars should fail" {
  compile_inline "char a := 'a' / 'c';"
  assert_failure
  assert_last_line "TypeError: / operator is not defined for type char."
}

@test "division with bools should fail" {
  compile_inline "bool a := true / true;"
  assert_failure
  assert_last_line "TypeError: / operator is not defined for type bool."
}

@test "division with strings should fail" {
  compile_inline 'string str := "foo" / "bar";'
  assert_failure
  assert_last_line "TypeError: / operator is not defined for type string."
}

@test "perform modulo correctly" {
  compile_inline "int a := 2 %% 3;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "modulo with chars should fail" {
  compile_inline "char a := 'a' %% 'c';"
  assert_failure
  assert_last_line "TypeError: % operator is not defined for type char."
}

@test "modulo with bools should fail" {
  compile_inline "bool a := true %% true;"
  assert_failure
  assert_last_line "TypeError: % operator is not defined for type bool."
}

@test "modulo with strings should fail" {
  compile_inline 'string str := "foo" %% "bar";'
  assert_failure
  assert_last_line "TypeError: % operator is not defined for type string."
}

@test "equality comparison between ints should succeed" {
  compile_inline 'bool a := 2 == 2;'
  assert_success
  assert_last_line "Passed type checking."
}

@test "equality comparison between int and char should fail" {
  compile_inline "bool a := 2 == 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot compare int with char."
}

@test "equality comparison between chars should succeed" {
  compile_inline "bool a := 'a' == 'a';"
  assert_success
  assert_last_line "Passed type checking."
}

@test "equality comparison between bools should succeed" {
  compile_inline "bool a := true == false;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "equality comparison between strings should fail" {
  compile_inline 'bool a := "a" == "s";'
  assert_failure
  assert_last_line "TypeError: == operator is not defined for type string."
}

@test "non-equality comparison between ints should succeed" {
  compile_inline 'bool a := 2 != 2;'
  assert_success
  assert_last_line "Passed type checking."
}

@test "non-equality comparison between int and char should fail" {
  compile_inline "bool a := 2 != 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot compare int with char."
}

@test "non-equality comparison between chars should succeed" {
  compile_inline "bool a := 'a' != 'a';"
  assert_success
  assert_last_line "Passed type checking."
}

@test "non-equality comparison between bools should succeed" {
  compile_inline "bool a := true != false;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "non-equality comparison between strings should fail" {
  compile_inline 'bool a := "a" != "s";'
  assert_failure
  assert_last_line "TypeError: != operator is not defined for type string."
}

@test "greater than comparison between ints should succeed" {
  skip "greater than comparison is broken"
  compile_inline "bool a := 2 > 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "greater than comparison between int and char should fail" {
  skip "greater than comparison is broken"
  compile_inline "bool a := 2 > 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot compare int with char."
}

@test "greater than comparison between chars should fail" {
  skip "greater than comparison is broken"
  compile_inline "bool a := 'a' > 'a';"
  assert_failure
  assert_last_line "TypeError: > operator is not defined for type char."
}

@test "greater than comparison between bools should fail" {
  skip "greater than comparison is broken"
  compile_inline "bool a := true > false;"
  assert_failure
  assert_last_line "TypeError: > operator is not defined for type bool."
}

@test "greater than comparison between strings should fail" {
  skip "greater than comparison is broken"
  compile_inline 'bool a := "a" > "s";'
  assert_failure
  assert_last_line "TypeError: > operator is not defined for type string."
}

@test "less than comparison between ints should succeed" {
  skip "less than comparison is broken"
  compile_inline "bool a := 2 < 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "less than comparison between int and char should fail" {
  skip "less than comparison is broken"
  compile_inline "bool a := 2 < 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot compare int with char."
}

@test "less than comparison between chars should fail" {
  skip "less than comparison is broken"
  compile_inline "bool a := 'a' < 'a';"
  assert_failure
  assert_last_line "TypeError: < operator is not defined for type char."
}

@test "less than comparison between bools should fail" {
  skip "less than comparison is broken"
  compile_inline "bool a := true < false;"
  assert_failure
  assert_last_line "TypeError: < operator is not defined for type bool."
}

@test "less than comparison between strings should fail" {
  skip "less than comparison is broken"
  compile_inline 'bool a := "a" < "s";'
  assert_failure
  assert_last_line "TypeError: < operator is not defined for type string."
}

@test "greater than or equal to comparison between ints should succeed" {
  compile_inline "bool a := 2 >= 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "greater than or equal to comparison between int and char should fail" {
  compile_inline "bool a := 2 >= 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot compare int with char."
}

@test "greater than or equal to comparison between chars should fail" {
  compile_inline "bool a := 'a' >= 'a';"
  assert_failure
  assert_last_line "TypeError: >= operator is not defined for type char."
}

@test "greater than or equal to comparison between bools should fail" {
  compile_inline "bool a := true >= false;"
  assert_failure
  assert_last_line "TypeError: >= operator is not defined for type bool."
}

@test "greater than or equal to comparison between strings should fail" {
  compile_inline 'bool a := "a" >= "s";'
  assert_failure
  assert_last_line "TypeError: >= operator is not defined for type string."
}

@test "less than or equal to comparison between ints should succeed" {
  compile_inline "bool a := 2 <= 2;"
  assert_success
  assert_last_line "Passed type checking."
}

@test "less than or equal to comparison between int and char should fail" {
  compile_inline "bool a := 2 <= 'a';"
  assert_failure
  assert_last_line "TypeError: Cannot compare int with char."
}

@test "less than or equal to comparison between chars should fail" {
  compile_inline "bool a := 'a' <= 'a';"
  assert_failure
  assert_last_line "TypeError: <= operator is not defined for type char."
}

@test "less than or equal to comparison between bools should fail" {
  compile_inline "bool a := true <= false;"
  assert_failure
  assert_last_line "TypeError: <= operator is not defined for type bool."
}

@test "less than or equal to comparison between strings should fail" {
  compile_inline 'bool a := "a" <= "s";'
  assert_failure
  assert_last_line "TypeError: <= operator is not defined for type string."
}