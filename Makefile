all:
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c tests.ml
	ocamlc -o tests ast.cmo lexer.cmo parser.cmo tests.cmo
