%{
open Syntax

exception Error
%}

%start <Syntax.service> parse

%token <string>ID
%token <string>STR
%token <string>META
%token <string>WHATEVER
%token <string>SIDENT
%token <string>BIDENT
%token <int>NUM
%token CONST HTML EQ
%token NAME TYPE SELECT INPUT CLOSING CLSELECT
%token SEMICOLON SHOW EXIT RETURN WHILE IF ELSE
%token LPAREN RPAREN LBRACKET RBRACKET LCURLY RCURLY
%token COMMA PERIOD
%token ISEQ NEQ LE GE LEQ GEQ BANG MINUS PLUS TIMES DIV MOD AND OR ASSIGN BPLUS BMINUS TRUE FALSE
%token INT BOOL STRING VOID TUPLE
%token SESSION SERVICE PLUG RECEIVE SCHEMA
%token EOF

%nonassoc THEN
%nonassoc ELSE
%left EQ
%left OR
%left AND
%left ISEQ NEQ
%left GE LE LEQ GEQ
%left BPLUS BMINUS ASSIGN
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS BANG

%%

parse:
| s = service EOF {s}

service:
| SERVICE LCURLY h=html+ s=schema* v=nevariables f=func* se=session+ RCURLY
    { Service (h,s,v,f,se) }
| SERVICE LCURLY h=html+ s=schema* f=func* se=session+ RCURLY
    { Service (h,s,[],f,se) }

html:
| CONST HTML id = identifier; EQ; hb = htmlbody*; SEMICOLON
    {Html (id, hb)}

htmlbody:
| id = identifier a = attribute* CLOSING
    {TagAttr (id, a)}
| id = SIDENT
    {TagClosed (IdentifierConst (id))}
| id = BIDENT
    {TagBracked (IdentifierConst (id))}
| m = META
    {Meta (MetaConst m)}
| INPUT; i = inputattr+ CLOSING
    {TagInput (i)}
| SELECT; i = inputattr+; CLOSING a = htmlbody* CLSELECT
    {TagSelect (i, a)}
| w = WHATEVER
    {Whatever (WhateverConst w)}

inputattr:
| NAME EQ a = attr
    {InputAttr a}
| TYPE EQ t = inputtype
    {InputType t}
| a = iattribute
    {Attribute a}

inputtype:
| i = STR
    {if i = "text" then
        Text
      else if i = "radio" then
        Radio
      else
        (print_endline ("Input type crashed on " ^ i); raise Error)
    }
| i = ID
    {if i = "text" then
        Text
      else if i = "radio" then
        Radio
      else
        (print_endline ("Input type crashed on " ^ i); raise Error)
    }

iattribute:
| a = iattr
    {Attr a}
| a1 = iattr EQ a2 = iattr
    {PairAttr (a1, a2)}

iattr:
| i = identifier
    {AttrID i}
| s = stringconst
    {AttrString s}
| s = BIDENT
    {AttrBident (IdentifierConst s)}

attribute:
| a = attr
    {Attr a}
| a1 = attr EQ a2 = attr
    {PairAttr (a1, a2)}

attr:
| i = identifier
    {AttrID i}
| s = stringconst
    {AttrString s}
| TYPE
    {AttrString (StringConst "type")}
| NAME
    {AttrString (StringConst "name")}
| s = BIDENT
    {AttrBident (IdentifierConst s)}

schema:
| SCHEMA i = identifier LCURLY f = field* RCURLY
    {Schema (i,f)}

field:
| s = simpletype i = identifier SEMICOLON
    {Field (s,i)}

nevariables:
| v = variable
    {[v]}
| vs = nevariables; v = variable
    {vs@[v]}

variable:
| t = tp i = identifiers SEMICOLON
    {Var (t,i)}

identifiers:
| i = identifier
    {[i]}
| i = identifier COMMA is = identifiers
    {i :: is}

simpletype:
| INT
    {IntType}
| BOOL
    {BoolType}
| STRING
    {StringType}
| VOID
    {VoidType}

tp:
| s = simpletype
    {s}
| TUPLE i = identifier
    {TupleType i}

func:
| t = tp i = identifier LPAREN a = arguments RPAREN c = compoundstm
    {Func (t,i,a,c)}

arguments:
| (* empty *)
    {[]}
| a = argument
    {[a]}
| a = argument COMMA n = arguments
    {a :: n}

argument: 
| t = tp i = identifier
    {Argument (t,i)}

session:
| SESSION i = identifier LPAREN RPAREN c = compoundstm
    {Session (i,c)}

stm:
| SEMICOLON 
    {EmptyStm}
| SHOW d = document; r = receive SEMICOLON 
    {Show (d, r)}
| EXIT d = document SEMICOLON 
    {Exit d}
| RETURN SEMICOLON 
    {ReturnVoid}
| RETURN e = exp SEMICOLON 
    {ReturnExp e}
| IF LPAREN e = exp RPAREN s = stm %prec THEN
    {
        let cmps = match s with 
            | CompStm x -> s
            | _ -> CompStm(Compound([], [s]))
        in
        StmIfElse(e,cmps,EmptyStm)
    }
| IF LPAREN e = exp RPAREN s1 = stm ELSE s2 = stm
    {
        let cmps1 = match s1 with 
            | CompStm x -> s1
            | _ -> CompStm(Compound([], [s1]))
        in
        let cmps2 = match s2 with 
            | CompStm x -> s2
            | _ -> CompStm(Compound([], [s2]))
        in
        StmIfElse(e,cmps1,cmps2)
    }
| WHILE LPAREN e = exp RPAREN s = stm 
    {
        let cmps = match s with 
            | CompStm x -> s
            | _ -> CompStm(Compound([], [s]))
        in
        StmWhile(e,cmps)
    }
| c = compoundstm 
    {CompStm c}
| e = exp SEMICOLON 
    {StmExp e}

document:
| i = identifier 
    {Document (i, [])}
| PLUG i = identifier LBRACKET p = plugs RBRACKET 
    {Document (i, p)}

receive:
| (* empty *) 
    {Receive []}
| RECEIVE LBRACKET i = inputs RBRACKET 
    {Receive i}

compoundstm:
| LCURLY v = nevariables; s = stm* RCURLY 
    {Compound (v, s)}
| LCURLY s = stm* RCURLY 
    {Compound ([], s)}

plugs:
| p = plug 
    {[p]}
| p = plug COMMA ps = plugs
    {p :: ps}

plug:
| i = identifier EQ e = exp 
    {Plug (i, e)}

inputs: 
| (* empty *) 
    {[]}
| i = input 
    {[i]}
| i = input COMMA is = inputs 
    {i :: is}

input:
| l = lvalue EQ i = identifier 
    {Input (l, i)}

exp:
| l = lvalue                    {LValue l}
| l = lvalue EQ e = exp         {LValueAssign (l, e)}
| e1 = exp ISEQ   e2 = exp      {Iseq   (e1, e2)}
| e1 = exp NEQ    e2 = exp      {Neq    (e1, e2)}
| e1 = exp LE     e2 = exp      {Le     (e1, e2)}
| e1 = exp GE     e2 = exp      {Ge     (e1, e2)}
| e1 = exp LEQ    e2 = exp      {Leq    (e1, e2)}
| e1 = exp GEQ    e2 = exp      {Geq    (e1, e2)}
| BANG e = exp                  {Bang e}
| MINUS e = exp                 {ParenExp(Minus (IntExp 0, e))} %prec UMINUS
| e1 = exp PLUS   e2 = exp      {Plus   (e1, e2)}
| e1 = exp MINUS  e2 = exp      {Minus  (e1, e2)}
| e1 = exp TIMES  e2 = exp      {Mult   (e1, e2)}
| e1 = exp DIV    e2 = exp      {Div    (e1, e2)}
| e1 = exp MOD    e2 = exp      {Mod    (e1, e2)}
| e1 = exp AND    e2 = exp      {And    (e1, e2)}
| e1 = exp OR     e2 = exp      {Or     (e1, e2)}
| e1 = exp ASSIGN e2 = exp      {Assign (e1, e2)}
| e  = exp BPLUS  i = identifier    
                                {BplusId  (e, [i])}
| e  = exp BMINUS i = identifier    
                                {BminusId (e, [i])}
| e  = exp BPLUS  LPAREN i = identifiers RPAREN 
                                {BplusId  (e, i)}
| e  = exp BMINUS LPAREN i = identifiers RPAREN 
                                {BminusId (e, i)}
| i = identifier LPAREN e = exps RPAREN 
                                {FunctionExp (i, e)}
| i = intconst                  {i}
| TRUE                          {BoolExp true}
| FALSE                         {BoolExp false}
| s = stringconst               {StringExp s}
| TUPLE LCURLY f = fieldvalues RCURLY   
                                {Tuple f}
| LPAREN e = exp RPAREN         {ParenExp e}

exps:
| (* empty *) 
    {[]}
| e = exp 
    {[e]}
| e = exp COMMA es = exps
    {e :: es}

lvalue:
| i  = identifier 
    {Lval i}
| i1 = identifier PERIOD i2 = identifier 
    {LvalStruct (i1, i2)}

fieldvalues:
| (* empty *) 
    {[]}
| f = fieldvalue 
    {[f]}
| f = fieldvalue COMMA fs = fieldvalues
    {f :: fs}

fieldvalue: 
| i = identifier EQ e = exp 
    {FieldValue (i, e)}

stringconst:
| str = STR
    {StringConst ("\""^str^"\"")}

intconst:
| i = NUM
    {IntExp i}

identifier:
| id = ID
    {IdentifierConst id}
