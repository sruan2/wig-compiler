{
open Parser
}

let tab = '\009'
let cr = '\013'
let lf = '\010'
let eol = cr | lf | cr lf

rule lex = parse
| [' ' '\t']  {lex lexbuf}
| eol         {EOL}
| '+'         {PLUS}
| '-'         {MINUS}
| '*'         {TIMES}
| '/'         {DIV}
| "abs"       {ABS}
| '('         {LPAREN}
| ')'         {RPAREN}
| "**"        {POW}
| '%'         {MOD}
| '0'         {INT (0)}
| ['1'-'9']['0'-'9']* as i 
  {INT (int_of_string i)}
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s 
  {ID (s)}
