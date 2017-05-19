(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | "for"          { KFor }
  | "func"         { KFunc }
  | "return"       { KReturn }
  | "const"        { KConst }
  | "let"          { KLet }
  | "if"           { KIf }
  | "else"         { KElse }
  | "void"         { TVoid }
  | "int"          { TInt }
  | "string"       { TString }
  | "bool"         { TBool }
  | ['a'-'z''A'-'Z']+ as lxm { VAR(lxm) }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LCURLY }
  | '}'            { RCURLY }
  | ','            { COMMA }
  | ':'            { COLON }
  | ';'            { SEMICOLON }
  | '='            { EQUALS }
  | '<'            { LT }
  | '>'            { GT }
  | ":="           { SETEQ }
  | "<="           { LTE }
  | ">="           { GTE }
  | "=="           { EQ }
  | eof            { EOF }
  | _              { raise (Failure "unknown token") }
