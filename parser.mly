/* File parser.mly */
%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LCURLY RCURLY
%token COLON
%token COMMA
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Ast.AST.exp> main
%%
main:
    expr EOL                { $1 }
;
expr:
    INT                     { Ast.AST.IntLit $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Ast.AST.(InfixOper (Plus, $1, $3)) }
  | expr MINUS expr         { Ast.AST.(InfixOper (Minus, $1, $3)) }
  | expr TIMES expr         { Ast.AST.(InfixOper (Times, $1, $3)) }
  | expr DIV expr           { Ast.AST.(InfixOper (Div, $1, $3)) }
  | MINUS expr %prec UMINUS { Ast.AST.(PrefixOper (Minus, $2)) }
;
