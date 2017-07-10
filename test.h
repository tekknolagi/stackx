// Simple test harness for SubX.

// Implement this function to initialize each test, and to clean up at the end.
void reset(void);

// Write your tests in functions with the precise first line:
//   `void test_xxxxx(void) {`
typedef void (*test_fn)(void);

// The 'build' script will auto-generate a list of such functions.
extern const test_fn Tests[];  // convention: global variables are capitalized

// Inside each test, signal failure by setting the global variable 'Passed' to
// false.
#include <stdbool.h>
extern bool Passed;

// A helper to help signal failure and print out failing tests.
#include <stdio.h>
#define CHECK(X) \
  if (Passed && !(X)) { \
    fprintf(stderr, "\nF - %s(%s:%d): %s\n", __FUNCTION__, __FILE__, __LINE__, #X); \
    Passed = false; \
    return;  /* stop at the very first failure inside a test */ \
  }

// To run all your tests, call `run_tests()`, say within main after parsing
// some commandline flag. It will print a dot for each passing test, and more
// verbose messages about failing tests.
int run_tests(void);
