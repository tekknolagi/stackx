### Prerequisites:

- [opam](https://opam.ocaml.org/)

### Steps to build:

1. `opam install menhir`
2. `make`

### Steps to run:

1. `./tests`

(should produce 0 output and 0 exit code)

See tests.ml to see what that binary does.

### In order to see AST output in console:

1. `ocaml` or `utop`
2. In the shell:

   1. `#load "lexer.cmo";;`
   2. `#load "parser.cmo";;`
   3. `#load "tests.cmo";;`
   4. `Tests.bigP "func a () : int { 1; }";;`

   ...you get the idea.
