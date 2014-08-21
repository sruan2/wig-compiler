open Ast

let prec e = match e with
   | Plus (e1, e2)    -> 0
   | Minus (e1, e2)   -> 0
   | Times (e1, e2)   -> 1
   | Div (e1, e2)     -> 1
   | Mod (e1, e2)     -> 1
   | Pow (e1, e2)     -> 2
   | Abs e'           -> 3
   | Int i            -> 3
   | Id s             -> 3

let prettyEXP e = 

	let isTimes e = 
		match e with 
			| Times (_,_) -> true
			| _           -> false

	in let isPlus e = 
		match e with 
			| Plus (_,_) -> true
			| _          -> false

	in let rec print e isWrap = 
    	if isWrap then print_char '(' else ();
  		( match e with
                   | Id s           ->   print_string s
                   | Int i          ->   print_int i
                   | Times (e1, e2) -> ( print e1 (not (isTimes e1) && (prec e > prec e1));
                                         print_char '*'; 
                                         print e2 (not (isTimes e2) && (prec e >= prec e2))
                                       )
                   | Div (e1, e2)   -> ( print e1 (prec e > prec e1);
                                         print_char '/';
                                         print e2 (prec e >= prec e2)
                                       )
                   | Mod (e1, e2)   -> ( print e1 (prec e > prec e1);
                                         print_char '%';
                                         print e2 (prec e >= prec e2)
                                       )
                   | Plus (e1, e2)  -> ( print e1 (not (isPlus e1) && (prec e > prec e1));
                                         print_char '+';
                                         print e2 (not (isPlus e2) && (prec e >= prec e2))
                                       )
                   | Minus (e1, e2) -> ( print e1 (prec e > prec e1);
                                         print_char '-';
                                         print e2 (prec e >= prec e2)
                                       )
                   | Pow (e1, e2)   -> ( print e1 (prec e >= prec e1);
                                         print_string "**";
                                         print e2 (prec e > prec e2)
                                       )
                   | Abs e'         -> ( print_string "abs";
                                         print e' true
                                       )
 		 );
    	if isWrap then print_char ')' else ();
	in print e false;;
