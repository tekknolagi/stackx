all: tests

.PHONY: all tests

tests:
	ocamlbuild -use-menhir tests.native

clean:
	ocamlbuild -clean
