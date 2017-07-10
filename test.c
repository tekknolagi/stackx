// Simple test harness for SubX.

#include <stdio.h>
#include <stdlib.h>

#include "test.h"

#include "function_list"  // list of function prototypes auto-generated in 'build'
const test_fn Tests[] = {
  #include "test_list"  // list of test function names auto-generated in 'build'
};

bool Passed = true;

int run_tests(void) {
  int num_failures = 0;
  for (size_t i = 0;  i < sizeof(Tests)/sizeof(Tests[0]);  ++i) {
    Passed = true;
    reset();
    (*Tests[i])();
    if (Passed) fprintf(stderr, ".");
    else ++num_failures;
  }
  fprintf(stderr, "\n");
  if (num_failures > 0) {
    fprintf(stderr, "%d failure%s\n", num_failures, (num_failures > 1 ? "s" : ""));
    return 1;
  }
  return 0;
}
