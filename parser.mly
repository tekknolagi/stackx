/* File parser.mly */
%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token KFor KFunc KReturn KConst KLet KIf KElse
%token TInt TString TBool
%token LPAREN RPAREN
%token LCURLY RCURLY
%token COLON SEMICOLON
%token COMMA
%token EQUALS
%token EOL EOF
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Ast.AST.statement> main
%%
main:
    stmt EOF { $1 }
;
expr:
    INT                     { Ast.AST.IntLit $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Ast.AST.(InfixOper (Plus, $1, $3)) }
  | expr MINUS expr         { Ast.AST.(InfixOper (Minus, $1, $3)) }
  | expr TIMES expr         { Ast.AST.(InfixOper (Times, $1, $3)) }
  | expr DIV expr           { Ast.AST.(InfixOper (Div, $1, $3)) }
  | VAR LPAREN actuals RPAREN { Ast.AST.Funcall ($1, $3) }
  | MINUS expr %prec UMINUS { Ast.AST.(PrefixOper (Minus, $2)) }
;
actuals:
  /* empty */       { [] }
  | expr            { [$1] }
  | expr COMMA actuals { $1 :: $3 }
;
type_:
    TInt { Ast.Type.Int }
  | TString { Ast.Type.String }
;
vardecl:
    VAR COLON type_ { $1, $3 }
;
stmt:
  expr SEMICOLON            { Ast.AST.Exp($1) }
  | KLet vardecl EQUALS expr SEMICOLON
      { Ast.AST.(Assignment (LLet, $2, $4)) }
  | KConst vardecl EQUALS expr SEMICOLON
      { Ast.AST.(Assignment (LConst, $2, $4)) }
  | KIf LPAREN expr RPAREN block KElse block
      { Ast.AST.IfElse ($3, $5, $7) }
  | KIf LPAREN expr RPAREN block
      { Ast.AST.If ($3, $5) }
  | KReturn expr SEMICOLON
      { Ast.AST.Return $2 }
;
stmts:
  /* empty */         { [] }
  | stmts stmt        { $1 @ [$2] }
;
block:
  LCURLY stmts RCURLY { $2 }
;
