### Prerequisites:

- [opam](https://opam.ocaml.org/)

### Steps to build:

1. `opam install menhir`
2. `make`

### Steps to run:

1. `./tests`

(should produce some error output -- tests that were expected to fail -- and 0
exit code)

See [tests.ml](tests.ml) to see what the binary does.

### In order to see AST output in console:

1. `make clean` and `make`
2. `ocaml` or `utop`
3. `./shell` -- keep in mind that this only accepts full programs as input, and
   each only on one line
