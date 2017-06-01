all: tests

.PHONY: all tests

tests: env.cmo ast.cmo lexer.cmo parser.cmo parser_message.cmo typed_ast.cmo tests.cmo
	ocamlfind ocamlc -package menhirLib -linkpkg -o tests \
			         env.cmo ast.cmo lexer.cmo parser.cmo parser_message.cmo \
					 typed_ast.cmo tests.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml

parser.cmi: ast.cmo parser.mli
	ocamlc -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	# --table used for incremental mode
	menhir --table parser.mly
	# menhir --explain parser.mly --compile-errors parser.messages > parser_message.ml

parser.cmi: parser.mli
	ocamlfind ocamlc -package menhirLib -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlfind ocamlc -package menhirLib -c parser.ml

%.cmo: %.ml
	ocamlc -c $<

%.cmi: *.mli
	ocamlc -c $<

clean:
	-rm ast.cmi ast.cmo typed_ast.cmi typed_ast.cmo
	-rm lexer.cmi lexer.cmo lexer.ml
	-rm parser.cmi parser.cmo parser.ml parser.mli
	-rm tests tests.cmi tests.cmo
