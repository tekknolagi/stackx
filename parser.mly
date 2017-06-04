/* File parser.mly */
%token <int> INT
%token <char> CHAR
%token <string> VAR
%token AMP
%token PLUS MINUS STAR DIV
%token LT LTE GT GTE EQ
%token OR AND
%token KFor KFunc KReturn KConst KLet KIf KElse
%token TVoid TInt TString TBool TChar
%token LPAREN RPAREN
%token LCURLY RCURLY
%token COLON SEMICOLON
%token COMMA
%token EQUALS
%token EOL EOF

%start main             /* the entry point */
%type <Ast.AST.t> main
%%

main:
  list(top_level_statement) EOF { Ast.AST.Prog $1 }
;

top_level_statement:
  | KFunc VAR LPAREN separated_list(COMMA, variable_declaration) RPAREN COLON type_expression block
    { Ast.AST.Fun ($2, $4, $7, $8) }

  | KConst variable_declaration EQUALS expression SEMICOLON
    { Ast.AST.Const ($2, $4) }
;

variable_declaration:
  | VAR COLON type_expression { $1, $3 }
;

block:
  | LCURLY list(statement) RCURLY { $2 }
;

statement:
  | expression SEMICOLON                                      { Ast.AST.Exp $1 }
  | KLet variable_declaration EQUALS expression SEMICOLON     { Ast.AST.(Let (LLet, $2, $4)) }
  | KConst variable_declaration EQUALS expression SEMICOLON   { Ast.AST.(Let (LConst, $2, $4)) }
  | if_statement                                              { $1 }
  | KReturn expression SEMICOLON                              { Ast.AST.Return $2 }
;

if_statement:
  | KIf LPAREN expression RPAREN block                      { Ast.AST.If ($3, $5) }
  | KIf LPAREN expression RPAREN block KElse block          { Ast.AST.IfElse ($3, $5, $7) }
  | KIf LPAREN expression RPAREN block KElse if_statement   { Ast.AST.IfElse ($3, $5, [$7]) }
;

expression:
  | assignment_expression   { $1 }
;

assignment_expression:
  | VAR EQUALS logical_or_expression  { Ast.AST.(SetEq ($1, $3)) }
  | logical_or_expression             { $1 }
;

logical_or_expression:
  | logical_and_expression OR logical_or_expression   { Ast.AST.(InfixOper (Or, $1, $3)) }
  | logical_and_expression                            { $1 }
;

logical_and_expression:
  | equal_not_expression AND logical_and_expression   { Ast.AST.(InfixOper (And, $1, $3)) }
  | equal_not_expression                              { $1 }
;

equal_not_expression:
  | less_equal_expression EQ equal_not_expression   { Ast.AST.(InfixOper (Eq, $1, $3)) }
  | less_equal_expression                           { $1 }
  (* TODO: Add the missing != operator here *)
;

less_equal_expression:
  | add_sub_expression LT less_equal_expression   { Ast.AST.(InfixOper (Lt, $1, $3)) }
  | add_sub_expression GT less_equal_expression   { Ast.AST.(InfixOper (Gt, $1, $3)) }
  | add_sub_expression LTE less_equal_expression  { Ast.AST.(InfixOper (Lte, $1, $3)) }
  | add_sub_expression GTE less_equal_expression  { Ast.AST.(InfixOper (Gte, $1, $3)) }
  | add_sub_expression                            { $1 }
;

add_sub_expression:
  | mult_div_expression PLUS add_sub_expression   { Ast.AST.(InfixOper (Plus, $1, $3)) }
  | mult_div_expression MINUS add_sub_expression  { Ast.AST.(InfixOper (Minus, $1, $3)) }
  | mult_div_expression                           { $1 }
;

mult_div_expression:
  | unary_expression STAR mult_div_expression   { Ast.AST.(InfixOper (Times, $1, $3)) }
  | unary_expression DIV mult_div_expression    { Ast.AST.(InfixOper (Div, $1, $3)) }
  | unary_expression                            { $1 }
;

unary_expression:
  | PLUS unary_expression    { Ast.AST.(PrefixOper (Plus, $2)) }
  | MINUS unary_expression   { Ast.AST.(PrefixOper (Minus, $2)) }
  | STAR unary_expression    { Ast.AST.Ref $2 }
  | AMP unary_expression     { Ast.AST.Deref $2 }
  | call_expression         { $1 }
;

call_expression:
  | call_expression LPAREN separated_list(COMMA, expression) RPAREN { Ast.AST.Funcall ($1, $3) }
  | literal                                                         { $1 }
;

literal:
  | INT                       { Ast.AST.IntLit $1 }
  | CHAR                      { Ast.AST.CharLit $1 }
  | VAR                       { Ast.AST.Var $1 }
  | LPAREN expression RPAREN  { $2 }
;

(* Type Grammar *)

type_expression:
  | type_identifier       { $1 }
  | type_expression STAR  { Ast.Type.Pointer $1 }
;

type_identifier:
  | TVoid     { Ast.Type.(Prim Void) }
  | TInt      { Ast.Type.(Prim Int) }
  | TString   { Ast.Type.(Prim String) }
  | TBool     { Ast.Type.(Prim Bool) }
  | TChar     { Ast.Type.(Prim Char) }
;
