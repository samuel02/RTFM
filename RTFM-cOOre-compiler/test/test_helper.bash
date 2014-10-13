#!/usr/bin/env bash

COMPILER="../src/Main.native -o ${BATS_TMPDIR}/${BATS_TEST_NAME}.core"

compile() {
  run ${COMPILER} -i $1
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

setup() {
  # Make sure the compiler is compiled
  make -C ../src rtfm_coore > /dev/null 2>&1
}