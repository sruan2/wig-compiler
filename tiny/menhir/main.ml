open Ast

let _ = print_endline "Enter an arithmetic expression:";;

let formula_of_string = Parser.parse Lexer.lex (Lexing.from_channel stdin);;

let s = formula_of_string in
let _ = print_string("Input: "); Pretty.prettyEXP s; print_newline() in
let e = print_string("Output: "); Eval.eval s in
Pretty.prettyEXP e; print_newline();;


