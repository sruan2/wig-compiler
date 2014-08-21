open Ast

exception DivZero
exception UndefinedPow

let rec power x y =
  if y = 0 then 1 else x * power x (y-1)

let rec eval e = match e with
  | Plus (e1, e2) -> 
    (match (eval e1, eval e2) with
     | (Int x, Int y) -> Int (x + y)
     | (Int 0, e') -> e'
     | (e', Int 0) -> e' 
     | (e', Int y) -> if y < 0 then Minus (e', Int (-y)) else Plus(e', Int y)
     | (e1', e2') -> Plus (e1', e2')
    )
  | Minus (e1, e2) -> 
    (match (eval e1, eval e2) with
     | (Int x, Int y) -> Int (x - y)
     | (e', Int 0) -> e'
     | (e1', e2') -> if e1'=e2' then Int 0 else Minus (e1', e2')
    )
  | Times (e1, e2) -> 
    (match (eval e1, eval e2) with
     | (e', Int 0) -> Int 0
     | (Int 0, e') -> Int 0 
     | (Int 1, e') -> e'
     | (e', Int 1) -> e'
     | (Int x, Int y) -> Int (x * y)
     | (e1', e2') -> Times (e1', e2')
    )
  | Div (e1, e2) ->
    (match (eval e1, eval e2) with
     | (e', Int 0) -> raise DivZero
     | (Int 0, e') -> Int 0
     | (e', Int 1) -> e'
     | (Int x, Int y) -> Int (x / y)
     | (e1', e2') -> if e1'=e2' then Int 1 else Div (e1', e2')
    )
  | Mod (e1, e2) ->
    (match (eval e1, eval e2) with
     | (e', Int 0) -> raise DivZero
     | (Int 0, e') -> Int 0
     | (Int x, Int y) -> Int (x mod y)
     | (e1', e2') -> if e1'=e2' then Int 0 else Mod (e1', e2')
    )
  | Pow (e1, e2) ->
    (match (eval e1, eval e2) with
     | (Int 0, Int 0) -> raise UndefinedPow
     | (Int 0, e') -> Int 0
     | (Int 1, e') -> Int 1
     | (e', Int 0) -> Int 1
     | (e', Int 1) -> e'
     | (Int x, Int y) -> Int (power x y)
     | (e1', e2') -> Pow (e1', e2')
    )
  | Abs e' -> 
    (match eval e' with
     | Int x -> Int (abs x)
     | e'' -> Abs e''
    )
  | _ -> e

