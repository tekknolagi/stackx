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
  | LCURLY block_body RCURLY { $2 }
;

block_body:
  | list(statement) { $1 }
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
;

logical_or_expression:
  | logical_and_expression OR logical_and_expression  { Ast.AST.(InfixOper (Or, $1, $3)) }
;

logical_and_expression:
  | equal_not_expression AND equal_not_expression   { Ast.AST.(InfixOper (And, $1, $3)) }
;

equal_not_expression:
  | less_equal_expression EQ less_equal_expression  { Ast.AST.(InfixOper (Eq, $1, $3)) }
  (* TODO: Add the missing != operator here *)
;

less_equal_expression:
  | add_sub_expression LT add_sub_expression    { Ast.AST.(InfixOper (Lt, $1, $3)) }
  | add_sub_expression GT add_sub_expression    { Ast.AST.(InfixOper (Gt, $1, $3)) }
  | add_sub_expression LTE add_sub_expression   { Ast.AST.(InfixOper (Lte, $1, $3)) }
  | add_sub_expression GTE add_sub_expression   { Ast.AST.(InfixOper (Gte, $1, $3)) }
;

add_sub_expression:
  | mult_div_expression PLUS mult_div_expression    { Ast.AST.(InfixOper (Plus, $1, $3)) }
  | mult_div_expression MINUS mult_div_expression   { Ast.AST.(InfixOper (Minus, $1, $3)) }
;

mult_div_expression:
  | unary_expression STAR unary_expression  { Ast.AST.(InfixOper (Times, $1, $3)) }
  | unary_expression DIV unary_expression   { Ast.AST.(InfixOper (Div, $1, $3)) }
;

unary_expression:
  | PLUS call_expression    { Ast.AST.(PrefixOper (Plus, $2)) }
  | MINUS call_expression   { Ast.AST.(PrefixOper (Minus, $2)) }
  | STAR call_expression    { Ast.AST.Ref $2 }
  | AMP call_expression     { Ast.AST.Deref $2 }
;

call_expression:
  | literal LPAREN separated_list(COMMA, expression) RPAREN { Ast.AST.Funcall ($1, $3) }
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
  | TInt      { Ast.Type.(Prim Void) }
  | TString   { Ast.Type.(Prim Void) }
  | TBool     { Ast.Type.(Prim Void) }
  | TChar     { Ast.Type.(Prim Void) }
;
