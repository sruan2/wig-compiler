%{
open Ast
%}

%start <Ast.exp> parse

%token <string>ID
%token <int>INT
%token PLUS MINUS DIV TIMES MOD POW ABS
%token EOL LPAREN RPAREN

%left PLUS MINUS
%left TIMES DIV MOD
%right POW
%left UMINUS
%%

parse:
| e = exp; EOL {e}

exp:
| id = ID                 
  {Ast.Id id}
| i = INT                 
  {Ast.Int i}
| e = exp; PLUS; f = exp
  {Ast.Plus (e,f)}
| e = exp; MINUS; f = exp 
  {Ast.Minus (e,f)}
| e = exp; TIMES; f = exp      
  {Ast.Times (e,f)}
| e = exp; DIV; f = exp        
  {Ast.Div (e,f)}
| MINUS; e = exp %prec UMINUS   
  {Ast.Minus (Ast.Int 0, e)}
| ABS; LPAREN; e = exp; RPAREN 
  {Ast.Abs e}
| e = exp; MOD; f = exp 
  {Ast.Mod (e, f)}
| e = exp; POW; f = exp 
  {Ast.Pow (e, f)}
| LPAREN; e = exp; RPAREN 
  {e}
