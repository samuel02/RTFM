#!/usr/bin/env bash

COMPILER="../src/Main.native -o ${BATS_TMPDIR}/${BATS_TEST_NAME}.core"

compile() {
  run ${COMPILER} -i fixtures/${1}.coore
}

compile_inline() {
  printf "class Root <> { Reset { $1 } }" > ${BATS_TMPDIR}/${BATS_TEST_NAME}.coore
  run ${COMPILER} -i ${BATS_TMPDIR}/${BATS_TEST_NAME}.coore
}

flunk() {
  { if [ "$#" -eq 0 ]; then cat -
    else echo "$@"
    fi
  }
  return 1
}

assert_success() {
  if [ "$status" -ne 0 ]; then
    flunk "command failed with exit status $status"
  fi
}

assert_failure() {
  if [ "$status" -eq 0 ]; then
    flunk "expected failed exit status"
  fi
}

assert_equal() {
  if [ "$1" != "$2" ]; then
    { echo "expected: $1"
      echo "actual:   $2"
    } | flunk
  fi
}

assert_last_line() {
  assert_equal "$1" ${lines[@]:(-1)}
}

assert_last_line_begins() {
  assert_equal "$1" ${lines[@]:(-1)}|cut -d' ' -f1
}

setup() {
  # Make sure the compiler is compiled
  make -C ../src rtfm_coore > /dev/null 2>&1
}