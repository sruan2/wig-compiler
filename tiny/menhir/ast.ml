type exp =
| Id    of string
| Int   of int
| Plus  of exp * exp
| Minus of exp * exp
| Times of exp * exp
| Div   of exp * exp
| Mod   of exp * exp
| Pow   of exp * exp
| Abs   of exp
