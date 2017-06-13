(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof

(*
let parse_number_type s =
  let get_prefix c = function | 'u' -> `U | 'i' -> `I | 'f' -> `F | _ -> failwith "invalid type prefix" in
  get_prefix s.[0], int_of_string @@ String.sub s (String.length s - 1)
*)
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
  | "string"       { TString }
  | "bool"         { TBool }
  | "char"         { TChar }
  | "u8"           { TU8 }
  | "i8"           { TI8 }
  | "u16"          { TU16 }
  | "i16"          { TI16 }
  | "u32"          { TU32 }
  | "i32"          { TI32 }
  | "u64"          { TU64 }
  | "i64"          { TI64 }
  | "f32"          { TF32 }
  | "f64"          { TF64 }
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
