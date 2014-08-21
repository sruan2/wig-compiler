{
open Parser
open Lexing
exception Error of string

type bufferstate =
| BaseLevel
| CommentLevel of int
| HTMLLevel of int
| InputAttrLevel of int * bool
| AttrIDLevel of int * bool

let commentdepth = function
  | CommentLevel n -> n
  | _ -> assert false

}

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let empty = [' ' '\t']
let newline = ['\n']
let str = ['"'](([^ '"']*(['\\']['"'])*)* as s)['"']
let num = ['1'-'9']['0'-'9']*
let catchall = [^ '>' '<' '=' '"' 'a'-'z' 'A'-'Z' '_' '0'-'9' ]+
let whatever = [^ '<' '>']+

rule lex state = parse
| empty {lex state lexbuf }
| newline {Lexing.new_line lexbuf; lex state lexbuf}
| "const"       {CONST}
| "html"        {HTML}
| '='           {EQ}
| "<html>"      {state := HTMLLevel 0; html 0 state lexbuf}
| ';'           {SEMICOLON}
| "show"        {SHOW}
| "plug"        {PLUG}
| "exit"        {EXIT}
| "return"      {RETURN}
| "if"          {IF}
| "else"        {ELSE}
| "while"       {WHILE}
| "receive"     {RECEIVE}

| ','           {COMMA}
| '.'           {PERIOD}
| '('           {LPAREN}
| ')'           {RPAREN}
| '['           {LBRACKET}
| ']'           {RBRACKET}
| '{'           {LCURLY}
| '}'           {RCURLY}
| "=="          {ISEQ}
| "!="          {NEQ}
| "<="          {LEQ}
| ">="          {GEQ}
| "<"           {LE}
| ">"           {GE}
| '!'           {BANG}
| '+'           {PLUS}
| '-'           {MINUS}
| '*'           {TIMES}
| '/'           {DIV}
| '%'           {MOD}
| "&&"          {AND}
| "||"          {OR}
| "<<"          {ASSIGN}
| "\\+"         {BPLUS}
| "\\-"         {BMINUS}
| "true"        {TRUE}
| "false"       {FALSE}
| "tuple"       {TUPLE}

| "schema"      {SCHEMA}
| "int"         {INT}
| "bool"        {BOOL}
| "string"      {STRING}
| "void"        {VOID}
| "session"     {SESSION}
| "service"     {SERVICE}

| '0'           {NUM (0)}

| "/*" {state := (CommentLevel 0); comment state lexbuf } (*Comments are just ignored *)
| "//"[^ '\n']*"\n" {Lexing.new_line lexbuf; lex state lexbuf}

| num as i      {NUM (int_of_string i)}
| id as s       {ID (s)}
| str   	{STR (s)}
| eof           {EOF}
| _
	{ raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and comment state = parse
| newline {Lexing.new_line lexbuf; comment state lexbuf}
| "*/" {let n = commentdepth !state in
        if n > 0 then 
          (state := (CommentLevel (n-1)); comment state lexbuf)
        else
          (state := BaseLevel; lex state lexbuf)}
| "/*" {state := (CommentLevel (commentdepth !state + 1)); comment state lexbuf}
| _    {comment state lexbuf}


and html n state = parse
| empty { html n state lexbuf }
| newline {Lexing.new_line lexbuf; html n state lexbuf}
| "<!--"([^ '>']* as s)"-->"
	{META (s)}
| "</html>"
	{if n = 0 then
	    (state := BaseLevel; lex state lexbuf)
	  else
	    raise (Error (Printf.sprintf "At offset %d: missing </select> token.\n" (Lexing.lexeme_start lexbuf)))}

| "<["(id as s)"]>"
	{BIDENT (s)}    
| "</"[' ' '\t' '\n']*"select"[' ' '\t' '\n']*">"
	 {if n = 0 then 
	     raise (Error (Printf.sprintf "At offset %d: extra </select> token.\n" (Lexing.lexeme_start lexbuf)))
	   else 
	     state := HTMLLevel (n-1); CLSELECT
	 }

| "</"([^ '<' '>']+ as s)">"
	{SIDENT (s)}
| "<"
	{state := InputAttrLevel (n, false); inputattr n false state lexbuf}

| whatever as s
	{WHATEVER s}

| ['>']
    {raise (Error (Printf.sprintf "At offset %d: extra > token.\n" (Lexing.lexeme_start lexbuf)))}

 and inputattr m b state = parse
| empty { inputattr m b state lexbuf }
| newline {Lexing.new_line lexbuf; inputattr m b state lexbuf}
| ['>']  
     {(if b = true then
	 (state := HTMLLevel (m+1))
       else
	 (state := HTMLLevel m)); CLOSING
	}
| "select"  {state := InputAttrLevel (m, true); SELECT}
| "input"   {INPUT}

| "name"    {NAME}
| "type"    {TYPE}
| ['=']     {state := AttrIDLevel (m, b); EQ}
| id as s
	{ID (s)}
| num as s
	{STR (s)}
| str
        {STR (s)}

| "<["(id as s)"]>"
	    {BIDENT s}

(*| catchall as s  
    {let pos = Lexing.lexeme_start_p lexbuf in
     raise (Error (Printf.sprintf "At line %d and offset %d: Read the catch-all pattern in inputattr: %s.\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (s)))}
*)

 and attrid m b state = parse
| empty { attrid m b state lexbuf }
| newline {Lexing.new_line lexbuf; attrid m b state lexbuf}
| id as s
	{state := InputAttrLevel (m, b); ID (s)}
| str
	{state := InputAttrLevel (m, b); STR (s)}
| '0'           {state := InputAttrLevel (m, b); STR "0"}
| num as i
	{state := InputAttrLevel (m, b); STR i}
| ['-'] num as i
	{state := InputAttrLevel (m, b); STR i}
| (num ['%']) as i
	{state := InputAttrLevel (m, b); STR i}
| "<["(id as s)"]>"
	    {state := InputAttrLevel (m, b); BIDENT s}

| [^ 'a'-'z' 'A'-'Z' '_' '0'-'9' '"' ] as s
    {raise (Error (Printf.sprintf "At offset %d: Invalid identifier: %s\n" (Lexing.lexeme_start lexbuf) (Char.escaped s)))}
