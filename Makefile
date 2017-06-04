all: tests

.PHONY: all tests

tests: varenv.cmo ast.cmo lexer.cmo parser.cmo typed_ast.cmo tests.cmo
	ocamlfind ocamlc -package menhirLib -linkpkg -o tests \
			         varenv.cmo ast.cmo lexer.cmo parser.cmo \
					 typed_ast.cmo tests.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml

parser.cmi: ast.cmo parser.mli
	ocamlc -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	# --table used for incremental mode
	menhir parser.mly
	# menhir --explain parser.mly --compile-errors parser.messages > parser_message.ml

%.cmo: %.ml
	ocamlc -c $<

%.cmi: *.mli
	ocamlc -c $<

clean:
	-rm -f varenv.cmi varenv.cmo
	-rm -f ast.cmi ast.cmo typed_ast.cmi typed_ast.cmo
	-rm -f lexer.cmi lexer.cmo lexer.ml
	-rm -f parser.cmi parser.cmo parser.ml parser.mli
	-rm -f tests tests.cmi tests.cmo
