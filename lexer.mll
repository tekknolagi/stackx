(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}

rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | "for"          { KFor }
  | "func"         { KFunc }
  | "return"       { KReturn }
  | "const"        { KConst }
  | "let"          { KLet }
  | "if"           { KIf }
  | "else"         { KElse }
  | "while"        { KWhile }
  | "true"         { KTrue }
  | "false"        { KFalse }
  | "void"         { TVoid }
  | "int"          { TInt }
  | "string"       { TString }
  | "bool"         { TBool }
  | "char"         { TChar }
  | '\'' _ '\'' as c { CHAR(c.[1]) }
  | ['a'-'z''A'-'Z']+ as lxm { VAR(lxm) }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '&'            { AMP }
  | '*'            { STAR }
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
  | "||"           { OR }
  | "&&"           { AND }
  | "<="           { LTE }
  | ">="           { GTE }
  | "=="           { EQ }
  | "!"            { NOT }
  | "!="           { NEQ }
  | eof            { EOF }
  | _              { raise (Failure "unknown token") }
