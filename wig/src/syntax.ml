type service =
| Service of htmls * schemas * variables * funcs * sessions

and htmls = html list
and htmlbodies = htmlbody list
and inputattrs = inputattr list
and attributes = attribute list
and schemas = schema list
and fields = field list
and identifiers = identifier list
and funcs = func list
and arguments = argument list
and sessions = session list
and stms = stm list
and plugs = plug list
and inputs = input list
and fieldvalues = fieldvalue list
and exps = exp list
and variables = variable list

and html =
| Html of identifier * htmlbodies

and htmlbody =
| TagAttr of identifier * attributes
| TagClosed of identifier
| TagBracked of identifier
| Whatever of whatever
| Meta of meta
| TagInput of inputattrs
| TagSelect of inputattrs * htmlbodies

and inputattr = 
| InputAttr of attr
| InputType of inputtype
| Attribute of attribute

and inputtype =
| Text
| Radio

and attribute =
| Attr of attr
| PairAttr of attr * attr

and attr = 
| AttrID of identifier
| AttrString of stringconst
| AttrBident of identifier

and schema =
| Schema of identifier * fields

and field =
| Field of tp * identifier

and variable =
| Var of tp * identifiers

and tp = 
| IntType
| BoolType
| StringType
| VoidType
| TupleType of identifier

and func =
| Func of tp * identifier * arguments * compoundstm

and argument =
| Argument of tp * identifier

and session =
| Session of identifier * compoundstm

and stm = 
| EmptyStm
| Show of document * receive
| Exit of document
| ReturnVoid
| ReturnExp of exp
| StmIfElse of exp * stm * stm
| StmWhile of exp * stm
| CompStm of compoundstm
| StmExp of exp

and document =
| Document of identifier * plugs

and receive =
| Receive of inputs

and compoundstm =
| Compound of variables * stms

and plug = 
| Plug of identifier * exp

and input = 
| Input of lvalue * identifier

and exp =
| LValue of lvalue
| LValueAssign of lvalue * exp
| Iseq of exp * exp
| Neq of exp * exp
| Le of exp * exp
| Ge of exp * exp
| Leq of exp * exp
| Geq of exp * exp
| Bang of exp
| Uminus of exp
| Plus of exp * exp
| Minus of exp * exp
| Mult of exp * exp
| Div of exp * exp
| Mod of exp * exp
| And of exp * exp
| Or of exp * exp
| Assign of exp * exp
| BplusId of exp * identifiers
| BminusId of exp * identifiers
| FunctionExp of identifier * exps
| IntExp of int
| BoolExp of bool
| StringExp of stringconst
| Tuple of fieldvalues
| ParenExp of exp
| TypedExp of exp * idtype

and idtype =
| Htmltp
| Functp of (identifier * idtype) list * idtype
| Sessiontp
| Voidtp
| Inttp
| Booltp
| Stringtp
| Tupletp of (identifier * idtype) list
| Nonetp

and lvalue = 
| Lval of identifier
| LvalStruct of identifier * identifier

and fieldvalue =
| FieldValue of identifier * exp

and stringconst =
| StringConst of string

and identifier =
| IdentifierConst of string

and meta = 
| MetaConst of string

and whatever =
| WhateverConst of string

