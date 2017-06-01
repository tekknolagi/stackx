all: tests

.PHONY: all tests

tests: ast.cmo lexer.cmo parser.cmo typed_ast.cmo tests.cmo
	ocamlc -o tests ast.cmo lexer.cmo parser.cmo typed_ast.cmo tests.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml

parser.cmi: ast.cmo parser.mli
	ocamlc -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	menhir parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: *.mli
	ocamlc -c $<

clean:
	-rm ast.cmi ast.cmo typed_ast.cmi typed_ast.cmo
	-rm lexer.cmi lexer.cmo lexer.ml
	-rm parser.cmi parser.cmo parser.ml parser.mli
	-rm tests tests.cmi tests.cmo
