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
3. In the shell:

   1. `#load "lexer.cmo";;`
   2. `#load "parser.cmo";;`
   3. `#load "tests.cmo";;`
   4. `Tests.parse "func a () : int { 1; }";;`

   ...you get the idea.
