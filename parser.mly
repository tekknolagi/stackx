/* File parser.mly */
%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token LT LTE GT GTE EQ
%token OR AND
%token KFor KFunc KReturn KConst KLet KIf KElse
%token TVoid TInt TString TBool
%token LPAREN RPAREN
%token LCURLY RCURLY
%token COLON SEMICOLON
%token COMMA
%token EQUALS SETEQ
%token EOL EOF

%right OR
%right AND
%right EQ
%right LT LTE GT GTE
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start main             /* the entry point */
%type <Ast.AST.t> main
%%
main:
  list(program_item) EOF { Ast.AST.Prog $1 }
;
expr:
    INT                     { Ast.AST.IntLit $1 }
  | VAR                     { Ast.AST.Var $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr AND expr           { Ast.AST.(CompOper (And, $1, $3)) }
  | expr OR expr            { Ast.AST.(CompOper (Or, $1, $3)) }
  | expr TIMES expr         { Ast.AST.(MathOper (Times, $1, $3)) }
  | expr DIV expr           { Ast.AST.(MathOper (Div, $1, $3)) }
  | expr PLUS expr          { Ast.AST.(MathOper (Plus, $1, $3)) }
  | expr MINUS expr         { Ast.AST.(MathOper (Minus, $1, $3)) }
  | expr LT expr            { Ast.AST.(CompOper (Lt, $1, $3)) }
  | expr GT expr            { Ast.AST.(CompOper (Gt, $1, $3)) }
  | expr LTE expr           { Ast.AST.(CompOper (Lte, $1, $3)) }
  | expr GTE expr           { Ast.AST.(CompOper (Gte, $1, $3)) }
  | expr EQ expr            { Ast.AST.(CompOper (Eq, $1, $3)) }
  | VAR LPAREN separated_list(COMMA, expr) RPAREN { Ast.AST.Funcall ($1, $3) }
  | MINUS expr %prec UMINUS { Ast.AST.(PrefixOper (Minus, $2)) }
;
type_:
    TVoid { Ast.Type.Void }
  | TInt { Ast.Type.Int }
  | TString { Ast.Type.String }
  | TBool { Ast.Type.Bool }
;
vardecl:
    VAR COLON type_ { $1, $3 }
;
const:
  | KConst vardecl EQUALS expr SEMICOLON
      { Ast.AST.(Assignment (LConst, $2, $4)) }
;
stmt:
  expr SEMICOLON
      { Ast.AST.Exp $1 }
  | KLet vardecl EQUALS expr SEMICOLON
      { Ast.AST.(Assignment (LLet, $2, $4)) }
  | const
      { $1 }
  | VAR SETEQ expr SEMICOLON
      { Ast.AST.SetEq ($1, $3) }
  | KIf LPAREN expr RPAREN block KElse block
      { Ast.AST.IfElse ($3, $5, $7) }
  | KIf LPAREN expr RPAREN block
      { Ast.AST.If ($3, $5) }
  | KReturn expr SEMICOLON
      { Ast.AST.Return $2 }
;
block:
  LCURLY list(stmt) RCURLY { $2 }
;
fundef:
  KFunc VAR LPAREN separated_list(COMMA, vardecl) RPAREN COLON type_ block
      { Ast.AST.Fun ($2, $4, $7, $8) }
;
program_item:
  fundef          { $1 }
  | KConst vardecl EQUALS expr SEMICOLON
      { Ast.AST.Const ($2, $4) }
;
