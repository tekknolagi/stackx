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

%left EQUALS
%right OR
%right AND
%right EQ
%right LT LTE GT GTE
%left PLUS MINUS        /* lowest precedence */
%left STAR DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%nonassoc AMP

%start main             /* the entry point */
%type <Ast.AST.t> main
%%
main:
  list(program_item) EOF { Ast.AST.Prog $1 }
;
expr:
    INT                     { Ast.AST.IntLit $1 }
  | CHAR                    { Ast.AST.CharLit $1 }
  | VAR                     { Ast.AST.Var $1 }
  | LPAREN expr RPAREN      { $2 }
  | AMP expr                { Ast.AST.Ref $2 }
  | STAR expr               { Ast.AST.Deref $2 }
  | expr AND expr           { Ast.AST.(InfixOper (And, $1, $3)) }
  | expr OR expr            { Ast.AST.(InfixOper (Or, $1, $3)) }
  | expr STAR expr          { Ast.AST.(InfixOper (Times, $1, $3)) }
  | expr DIV expr           { Ast.AST.(InfixOper (Div, $1, $3)) }
  | expr PLUS expr          { Ast.AST.(InfixOper (Plus, $1, $3)) }
  | expr MINUS expr         { Ast.AST.(InfixOper (Minus, $1, $3)) }
  | expr LT expr            { Ast.AST.(InfixOper (Lt, $1, $3)) }
  | expr GT expr            { Ast.AST.(InfixOper (Gt, $1, $3)) }
  | expr LTE expr           { Ast.AST.(InfixOper (Lte, $1, $3)) }
  | expr GTE expr           { Ast.AST.(InfixOper (Gte, $1, $3)) }
  | expr EQ expr            { Ast.AST.(InfixOper (Eq, $1, $3)) }
  | VAR EQUALS expr         { Ast.AST.(SetEq ($1, $3)) }
  | VAR LPAREN separated_list(COMMA, expr) RPAREN
        { Ast.AST.(Funcall (Var $1, $3)) }
  | LPAREN expr RPAREN LPAREN separated_list(COMMA, expr) RPAREN
        { Ast.AST.Funcall ($2, $5) }
  | MINUS expr %prec UMINUS { Ast.AST.(PrefixOper (Minus, $2)) }
;
primty:
    TVoid       { Ast.Type.(Prim Void) }
  | TInt        { Ast.Type.(Prim Int) }
  | TString     { Ast.Type.(Prim String) }
  | TBool       { Ast.Type.(Prim Bool) }
  | TChar       { Ast.Type.(Prim Char) }
;
ty:
  | primty      { $1 }
  | ty STAR     { Ast.Type.Pointer $1 }
;
vardecl:
    VAR COLON ty { $1, $3 }
;
stmt:
  expr SEMICOLON
      { Ast.AST.Exp $1 }
  | KLet vardecl EQUALS expr SEMICOLON
      { Ast.AST.(Let (LLet, $2, $4)) }
  | KConst vardecl EQUALS expr SEMICOLON
      { Ast.AST.(Let (LConst, $2, $4)) }
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
program_item:
    KFunc VAR LPAREN separated_list(COMMA, vardecl) RPAREN COLON ty block
      { Ast.AST.Fun ($2, $4, $7, $8) }
  | KConst vardecl EQUALS expr SEMICOLON
      { Ast.AST.Const ($2, $4) }
;
