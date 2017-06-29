/* File parser.mly */
%token <int> INT
%token <char> CHAR
%token <string> VAR
%token AMP
%token PLUS MINUS STAR DIV
%token LT LTE GT GTE EQ NOT NEQ
%token OR AND
%token KFor KFunc KReturn KConst KLet KIf KElse KWhile
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
  | KFunc VAR LPAREN separated_list(COMMA, variable_decl) RPAREN COLON type_exp block
    { Ast.AST.Fun ($2, $4, $7, $8) }
  | KConst variable_decl EQUALS exp SEMICOLON
    { Ast.AST.Const ($2, $4) }
;

variable_decl:
  | VAR COLON type_exp { $1, $3 }
;

block:
  | LCURLY list(statement) RCURLY { $2 }
;

statement:
  | exp SEMICOLON                               { Ast.AST.Exp $1 }
  | KLet variable_decl EQUALS exp SEMICOLON     { Ast.AST.(Let (LLet, $2, $4)) }
  | KConst variable_decl EQUALS exp SEMICOLON   { Ast.AST.(Let (LConst, $2, $4)) }
  | if_statement                                { $1 }
  | KWhile LPAREN exp RPAREN block              { Ast.AST.(While ($3, $5)) }
  | KReturn exp SEMICOLON                       { Ast.AST.Return $2 }
;

if_statement:
  | KIf LPAREN exp RPAREN block                      { Ast.AST.If ($3, $5) }
  | KIf LPAREN exp RPAREN block KElse block          { Ast.AST.IfElse ($3, $5, $7) }
  | KIf LPAREN exp RPAREN block KElse if_statement   { Ast.AST.IfElse ($3, $5, [$7]) }
;

exp:
  | assignment_exp   { $1 }
;

assignment_exp:
  | VAR EQUALS logical_or_exp  { Ast.AST.(SetEq ($1, $3)) }
  | logical_or_exp             { $1 }
;

logical_or_exp:
  | logical_or_exp OR logical_and_exp   { Ast.AST.(InfixOper (Or, $1, $3)) }
  | logical_and_exp                     { $1 }
;

logical_and_exp:
  | logical_and_exp AND equal_not_exp   { Ast.AST.(InfixOper (And, $1, $3)) }
  | equal_not_exp                       { $1 }
;

equal_not_exp:
  | equal_not_exp EQ less_equal_exp   { Ast.AST.(InfixOper (Eq, $1, $3)) }
  | equal_not_exp NEQ less_equal_exp  { Ast.AST.(PrefixOper (Not, InfixOper (Eq, $1, $3))) }
  | less_equal_exp                    { $1 }
;

less_equal_exp:
  | less_equal_exp LT add_sub_exp   { Ast.AST.(InfixOper (Lt, $1, $3)) }
  | less_equal_exp GT add_sub_exp   { Ast.AST.(InfixOper (Gt, $1, $3)) }
  | less_equal_exp LTE add_sub_exp  { Ast.AST.(InfixOper (Lte, $1, $3)) }
  | less_equal_exp GTE add_sub_exp  { Ast.AST.(InfixOper (Gte, $1, $3)) }
  | add_sub_exp                     { $1 }
;

add_sub_exp:
  | add_sub_exp PLUS mult_div_exp   { Ast.AST.(InfixOper (Plus, $1, $3)) }
  | add_sub_exp MINUS mult_div_exp  { Ast.AST.(InfixOper (Minus, $1, $3)) }
  | mult_div_exp                    { $1 }
;

mult_div_exp:
  | mult_div_exp STAR unary_exp   { Ast.AST.(InfixOper (Times, $1, $3)) }
  | mult_div_exp DIV unary_exp    { Ast.AST.(InfixOper (Div, $1, $3)) }
  | unary_exp                     { $1 }
;

unary_exp:
  | PLUS unary_exp    { Ast.AST.(PrefixOper (Plus, $2)) }
  | MINUS unary_exp   { Ast.AST.(PrefixOper (Minus, $2)) }
  | NOT unary_exp     { Ast.AST.(PrefixOper (Not, $2)) }
  | STAR unary_exp    { Ast.AST.Deref $2 }
  | AMP unary_exp     { Ast.AST.Ref $2 }
  | call_exp          { $1 }
;

call_exp:
  | call_exp LPAREN separated_list(COMMA, exp) RPAREN
    { Ast.AST.Funcall ($1, $3) }
  | literal
    { $1 }
;

literal:
  | INT                  { Ast.AST.IntLit $1 }
  | CHAR                 { Ast.AST.CharLit $1 }
  | VAR                  { Ast.AST.Var $1 }
  | LPAREN exp RPAREN    { $2 }
;

(* Type Grammar *)

type_exp:
  | type_identifier       { $1 }
  | type_exp STAR         { Ast.Type.Pointer $1 }
;

type_identifier:
  | TVoid     { Ast.Type.(Prim Void) }
  | TInt      { Ast.Type.(Prim Int) }
  | TString   { Ast.Type.(Prim String) }
  | TBool     { Ast.Type.(Prim Bool) }
  | TChar     { Ast.Type.(Prim Char) }
;
