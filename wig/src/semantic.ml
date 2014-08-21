open Syntax

type hashtable = (identifier, symbol) Hashtbl.t
and tree = Node of hashtable * tree list ref
and symbol = Symbol of symboltype * symboldata * idtype

and symboltype =
  | HtmlSymbol
  | BodyTagSymbol
  | BodyVarSymbol
  | SchemaSymbol
  | FieldSymbol
  | VarSymbol
  | FuncSymbol
  | SessionSymbol

and symboldata =
  | NoData
  | HashRef of hashtable
  | IsDeclared of bool
  | StageNum of int ref
  | HtmlData of hashtable * (identifier list)

exception Duplicate of identifier
exception UndefType of identifier
exception MissingDeclaration of identifier
exception FoundItem of symbol
exception FoundType of idtype
exception FoundStage of int ref
exception FoundSymboltp of symboltype
exception FoundHtmlht of hashtable
exception FoundHtmltaglist of identifier list
exception FoundSchema of hashtable
exception MissingHashRef
exception MissingHtmlTaglist
exception MissingBodyVar
exception SchemaMemberMissing of identifier
exception SymbolTableFailed
(* the first is the current type, and the second is the expected type *)
exception TypeError of exp * string * idtype * idtype
exception StmTypeError of stm * string * idtype * idtype
exception DocumentTypeError of document * string * idtype * idtype
exception TupleTypeError of idtype * idtype * idtype * idtype
exception TupleBIdError of string * idtype * identifier
exception FuncTypeError of identifier

exception TupleBPlusError
exception TupleBminusError

(* for debugging *)
let symboltpToStr sym = match sym with
  | HtmlSymbol      -> "HtmlSymbol"
  | BodyTagSymbol   -> "BodyTagSymbol"
  | BodyVarSymbol   -> "BodyVarSymbol"
  | SchemaSymbol    -> "SchemaSymbol"
  | FieldSymbol     -> "FieldSymbol"
  | VarSymbol       -> "VarSymbol"
  | FuncSymbol      -> "FuncSymbol"
  | SessionSymbol   -> "SessionSymbol"

let rec idtypeToString idtp =
  match idtp with
  | Htmltp          -> "Html type"
  | Functp (l,idtp')->
      "Function (Return type: "^ idtypeToString idtp' ^
      ", Arguments type: " ^ argListToString l
  | Sessiontp       -> "Session type"
  | Voidtp          -> "Void type"
  | Inttp           -> "Int type"
  | Booltp          -> "Bool type"
  | Stringtp        -> "String type"
  | Tupletp l       -> "Tuple type (" ^ argListToString l
  | Nonetp          -> "None type"

  and argListToString arglist = match arglist with
    | []  -> ""
    | (_,hidtp)::tl ->
    	match tl with
    		|[] -> idtypeToString hidtp ^ ")"
    		|_ ->  (idtypeToString hidtp) ^", "^ (argListToString tl)

  and idToString id =
    let IdentifierConst(id') = id in id'

  and idsToString ids = match ids with
    | [] -> 
        ""
    | x :: xs -> (idsToString xs)^","^(idToString x)

  and lvalueToString lvalue =  match lvalue with
    | Lval id -> idToString id
    | LvalStruct (id1,id2) -> (idToString id1)^"."^(idToString id2)

  and stmToString stm = match stm with
    | ReturnVoid -> "return"
    | ReturnExp exp -> "return " ^ (expToString exp)
    | StmIfElse (exp, stm1, stm2) -> "if (" ^ (expToString exp) ^ ") else ..."
    | StmWhile (exp, stm1) -> "while (" ^ (expToString exp) ^ ") ... "
    | _ -> ""

  and expToString exp = match exp with
    | LValue lvalue -> 
        lvalueToString lvalue 
    | LValueAssign (lvalue, exp') -> 
        (lvalueToString lvalue) ^ "=" ^ (expToString exp')
    | Iseq(exp1, exp2)-> 
        (expToString exp1) ^ "==" ^ (expToString exp2)
    | Neq (exp1, exp2) -> 
        (expToString exp1) ^ "!=" ^ (expToString exp2)
    | Le (exp1, exp2) ->      
        (expToString exp1) ^ "<" ^ (expToString exp2)
    | Ge (exp1, exp2) ->
        (expToString exp1) ^ ">" ^ (expToString exp2)
    | Leq (exp1, exp2) ->
        (expToString exp1) ^ "<=" ^ (expToString exp2)
    | Geq (exp1, exp2) ->
        (expToString exp1) ^ ">=" ^ (expToString exp2)
    | Bang exp' ->
        "!" ^ (expToString exp')
    | Uminus exp' ->
        "-" ^ (expToString exp')
    | Plus (exp1, exp2) ->
        (expToString exp1) ^ "+" ^ (expToString exp2)
    | Minus (exp1, exp2) ->
        (expToString exp1) ^ "-" ^ (expToString exp2)
    | Mult (exp1, exp2) ->
        (expToString exp1) ^ "*" ^ (expToString exp2)
    | Div (exp1, exp2) ->
        (expToString exp1) ^ "/" ^ (expToString exp2)
    | Mod (exp1, exp2) ->
        (expToString exp1) ^ "%" ^ (expToString exp2)
    | And (exp1, exp2) ->
        (expToString exp1) ^ "&&" ^ (expToString exp2)
    | Or (exp1, exp2) ->
        (expToString exp1) ^ "||" ^ (expToString exp2)
    | Assign (exp1, exp2) ->
        (expToString exp1) ^ "<<" ^ (expToString exp2)
    | BplusId (exp1, ids) -> 
        (expToString exp1) ^ "\\+" ^ (idsToString ids)
    | BminusId (exp1, ids) ->
        (expToString exp1) ^ "\\-" ^ (idsToString ids)
    | FunctionExp (id, exps2) ->
        (idToString id) ^ " (" ^ (expsToString exps2) ^ ")"
    | IntExp intconst ->
        string_of_int intconst
    | BoolExp boolean ->
        string_of_bool boolean
    | StringExp (StringConst stringconst) ->
        stringconst
    | Tuple fieldvalues ->
        fieldvaluesToString fieldvalues
    | ParenExp exp' ->
        "(" ^ (expToString exp') ^")"
    | TypedExp (exp',_) ->
        expToString exp'

  and expsToString exps = match exps with
    | [] -> 
        ""
    | x :: xs -> 
        (expToString x) ^ "," ^ (expsToString xs)

  and fieldvaluesToString fieldvalues = match fieldvalues with
    | [] -> 
        ""
    | FieldValue (id,exp) :: xs -> 
        "(" ^ idToString id ^ "," ^expToString exp ^")" ^ (fieldvaluesToString xs)

  and sameType (tp1,tp2) = 
    match tp1 with
      | Tupletp tuple1 -> (match tp2 with
          | Tupletp tuple2 ->
              sameIdList(tuple1, tuple2)
          | _ -> false)
      | _ -> tp1==tp2

  and sameIdList (list1, list2) = match (list1, list2) with 
    | ((_,tp1) :: xs, (_, tp2) :: ys)-> if tp1==tp2 then sameIdList(xs,ys) else false
    | ([], []) -> true
    | (_, _) -> false

  and sameId (id1,id2) = 
    let IdentifierConst(str1) = id1 in
    let IdentifierConst(str2) = id2 in
    String.compare str1 str2 == 0

  and typeof exp = match exp with
    | TypedExp (e,i) -> i
    | _ -> Nonetp

  and countPlugs cstm = 
    let rec plugsInStms stms = (match stms with
      | [] -> 0
      | x :: xs -> (plugsInStm x) + (plugsInStms xs))
    and plugsInStm stm = match stm with
      | EmptyStm -> 0
      | Show (document, receive) -> 1
      | Exit document -> 1
      | ReturnVoid -> 0
      | ReturnExp exp -> 0
      | StmIfElse (exp, stm', stm'') -> plugsInStm stm' + plugsInStm stm''
      | StmWhile (exp, stm') -> plugsInStm stm'
      | CompStm cstm' -> countPlugs cstm'
      | StmExp exp -> 0
    in
    let Compound (_, stms) = cstm in plugsInStms stms


