open Syntax

exception IndentationFailure

let buf = Buffer.create 16

let printBuffer unit =
  print_string (Buffer.contents buf)
  
let prettySyntax s = 

  let indentLevel = ref 0 in
  let tabUp () = 
    indentLevel := (!indentLevel + 1) in
  let tabDown () = 
    if !indentLevel==0 then raise IndentationFailure
    else indentLevel := (!indentLevel - 1) in
  let addTabs () = 
    let rec singleTab n = match n with
      | 0 -> ()
      | _ -> Buffer.add_string buf "    "; singleTab (n-1)
    in
    singleTab !indentLevel
  in

(* Each constructor has a 'prettify' function *)

  let rec prettyTp tp = match tp with
    | TupleType (identifier) -> 
        Buffer.add_string buf "tuple ";
        prettyIdentifier identifier;
        Buffer.add_string buf " "
    | IntType -> 
        Buffer.add_string buf "int "
    | BoolType -> 
        Buffer.add_string buf "bool "
    | StringType -> 
        Buffer.add_string buf "string "
    | VoidType -> 
        Buffer.add_string buf "void "

  and prettyAttributes attributes = match attributes with
    | [] -> 
        ()
    | x :: xs -> 
        prettyAttribute x;
        prettyAttributes xs

  and prettyAttr attr = match attr with
    | AttrID (identifier) -> 
        prettyIdentifier identifier
    | AttrString (strconst) -> 
        prettyString strconst
    | AttrBident (identifier) -> 
        Buffer.add_string buf "<[";
        prettyIdentifier identifier;
        Buffer.add_string buf "]>"

  and prettyAttribute attribute = match attribute with   
    | Attr attr -> 
        Buffer.add_string buf " ";
        prettyAttr attr
    | PairAttr (attr1, attr2) -> 
        Buffer.add_string buf " ";
        prettyAttr attr1; 
        Buffer.add_string buf "=";
        prettyAttr attr2

  and prettyInputAttrs s = match s with
    | [] ->
        ()
    | x :: xs -> 
        prettyInputAttr x;
        prettyInputAttrs xs

  and prettyInputAttr a = match a with 
    | InputAttr attr ->
        Buffer.add_string buf " name = ";
        prettyAttr attr
    | InputType inputtype ->
        Buffer.add_string buf " type = ";
        prettyInputType inputtype
    | Attribute attribute ->
        prettyAttribute attribute

  and prettyInputType t = match t with
    | Text ->
        Buffer.add_string buf "text "
    | Radio ->
        Buffer.add_string buf "radio "

  and prettyHtmlbody htmlbody = match htmlbody with
    | TagAttr (identifier, attributes) -> 
        Buffer.add_string buf "<";
        prettyIdentifier identifier;
        prettyAttributes attributes; 
        Buffer.add_string buf ">" 
    | TagClosed (identifier) -> 
        Buffer.add_string buf "</";
        prettyIdentifier identifier;
        Buffer.add_string buf ">"
    | TagBracked (identifier) -> 
        Buffer.add_string buf "<[";
        prettyIdentifier identifier;
        Buffer.add_string buf "]>"
    | Whatever (WhateverConst s) -> 
        Buffer.add_string buf s;
    | Meta (MetaConst s) -> 
        Buffer.add_string buf "<!--";
        Buffer.add_string buf s;
        Buffer.add_string buf "-->"
    | TagInput inputattrs -> 
        Buffer.add_string buf "<input ";
        prettyInputAttrs inputattrs;
        Buffer.add_string buf ">"
    | TagSelect (inputattrs, htmlbodies) -> 
        Buffer.add_string buf "<select ";
        prettyInputAttrs inputattrs;
        Buffer.add_string buf ">";
        prettyHtmlbodies htmlbodies;
        Buffer.add_string buf "</select>"

  and prettyHtmlbodies htmlbodies = match htmlbodies with
    | [] -> 
        ()
    | x :: xs -> 
        prettyHtmlbody x;
        prettyHtmlbodies xs

  and prettyHtml html = match html with
    | Html (identifier, htmlbodies) ->
        addTabs();
        Buffer.add_string buf "const html ";
        prettyIdentifier identifier;
        Buffer.add_string buf " = <html>";
        prettyHtmlbodies htmlbodies;
        Buffer.add_string buf "</html>;\n";

  and prettyHtmls htmls = match htmls with
    | [] ->
        ()
    | x :: xs -> 
        prettyHtml x;
        prettyHtmls xs;

  and prettyFields fields = match fields with
    | [] ->
        ()
    | x :: xs ->
        addTabs();
        prettyField x;
        prettyFields xs 

  and prettyField field = match field with
    | Field (simpletype, identifier) ->
        prettyTp simpletype;
        prettyIdentifier identifier;
        Buffer.add_string buf ";\n";

  and prettySchemas schemas = match schemas with
    | [] ->
        ()
    | x :: xs -> 
      prettySchema x; 
      prettySchemas xs

  and prettySchema schema = match schema with
    | Schema (identifier, fields) ->
        addTabs();
        Buffer.add_string buf "schema ";
        prettyIdentifier identifier;
        Buffer.add_string buf "\n";
        addTabs();
        Buffer.add_string buf "{\n";
        tabUp();
        prettyFields fields;
        tabDown();
        addTabs();
        Buffer.add_string buf "}\n"

  and prettyIdentifiers identifiers = match identifiers with
    | [] ->
        ()  
    | [x] ->
        prettyIdentifier x
    | x :: xs -> 
        prettyIdentifier x; 
        Buffer.add_string buf ",";
        prettyIdentifiers xs

  and prettyIdentifier identifier = match identifier with
    | IdentifierConst(id) ->
        Buffer.add_string buf id;

  and prettyString str = match str with
    | StringConst(strconst) ->
        Buffer.add_string buf strconst;

  and prettyVariables variables = match variables with
    | [] -> 
        ()
    | variable :: consvariable -> 
        prettyVariable variable;
        prettyVariables consvariable

  and prettyVariable variable = match variable with
    | Var (tp, identifiers) ->
        addTabs();
        prettyTp tp; 
        prettyIdentifiers identifiers;
        Buffer.add_string buf ";\n"

  and prettyArguments arguments = match arguments with
    | [] -> 
        ()
    | x :: [] -> 
        prettyArgument x
    | x :: xs -> 
        prettyArgument x; 
        Buffer.add_string buf ",";
        prettyArguments xs

  and prettyArgument argument = match argument with
    | Argument (tp, identifier) ->
        prettyTp tp; 
        prettyIdentifier identifier

  and prettyExp exp = match exp with
    | LValue lvalue -> 
        prettyLvalue lvalue;
    | LValueAssign (lvalue, exp) -> 
        prettyLvalue lvalue;
        Buffer.add_string buf "="; 
        prettyExp exp;
    | Iseq (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "=="; 
        prettyExp exp2
    | Neq (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "!="; 
        prettyExp exp2
    | Le (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "<"; 
        prettyExp exp2
    | Ge (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf ">"; 
        prettyExp exp2
    | Leq (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "<="; 
        prettyExp exp2
    | Geq (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf ">="; 
        prettyExp exp2
    | Bang exp -> 
        Buffer.add_string buf "!"; 
        prettyExp exp 
    | Uminus exp -> 
        Buffer.add_string buf "-"; 
        prettyExp exp
    | Plus (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "+"; 
        prettyExp exp2
    | Minus (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "-"; 
        prettyExp exp2
    | Mult (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "*"; 
        prettyExp exp2
    | Div (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "/"; 
        prettyExp exp2
    | Mod (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "%"; 
        prettyExp exp2
    | And (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "&&"; 
        prettyExp exp2
    | Or (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "||"; 
        prettyExp exp2
    | Assign (exp1, exp2) -> 
        prettyExp exp1; 
        Buffer.add_string buf "<<"; 
        prettyExp exp2
    | BplusId (exp, identifiers) -> 
        prettyExp exp; 
        Buffer.add_string buf "\\+ (";
        prettyIdentifiers identifiers; 
        Buffer.add_string buf ")" 
    | BminusId (exp, identifiers) -> 
        prettyExp exp; 
        Buffer.add_string buf "\\- ("; 
        prettyIdentifiers identifiers; 
        Buffer.add_string buf ")" 
    | FunctionExp (identifier, exps) -> 
        prettyIdentifier identifier;
        Buffer.add_string buf "("; 
        prettyExps exps; 
        Buffer.add_string buf ")"
    | IntExp i -> 
        Buffer.add_string buf (string_of_int i)
    | BoolExp b -> 
        Buffer.add_string buf (string_of_bool b)
    | StringExp (strconst) -> 
        prettyString strconst
    | Tuple fieldvalues -> 
        Buffer.add_string buf "tuple {"; 
        prettyFieldvalues fieldvalues; 
        Buffer.add_string buf "}" 
    | ParenExp exp -> 
        Buffer.add_string buf "("; 
        prettyExp exp; 
        Buffer.add_string buf ")"
    | TypedExp (exp,t) ->
        prettyExp exp;
        Buffer.add_string buf " /* ";
        Buffer.add_string buf (Semantic.idtypeToString t);
        Buffer.add_string buf " */ ";

  and prettyFieldvalues fieldvalues = match fieldvalues with
    | [] -> 
        ()
    | [x] ->     
        prettyFieldvalue x     
    | x :: xs -> 
        prettyFieldvalue x; 
        Buffer.add_string buf ",";
        prettyFieldvalues xs

  and prettyFieldvalue fieldvalue = match fieldvalue with
    | FieldValue (identifier, exp) ->
        prettyIdentifier identifier;
        Buffer.add_string buf "="; 
        prettyExp exp

  and prettyExps exps = match exps with
    | [] -> 
        ()
    | [x] -> 
        prettyExp x
    | x :: xs -> 
        prettyExp x; 
        Buffer.add_string buf ",";
        prettyExps xs

  and prettyStms stms = match stms with
    | [] -> 
        ()
    | x :: xs -> 
        prettyStm x;
        prettyStms xs

  and prettyCompoundstm compoundstm = match compoundstm with              
    | Compound (variables, stms) -> 
        addTabs();
        Buffer.add_string buf "{\n";
        tabUp();
        prettyVariables variables; 
        prettyStms stms;
        tabDown();
        addTabs();
        Buffer.add_string buf "}\n";

  and prettyStm stm = match stm with
    | EmptyStm -> 
        Buffer.add_string buf ";\n"
    | Show (document, receive) -> 
        addTabs();
        Buffer.add_string buf "show "; 
        prettyDocument document;
        prettyReceive receive;
        Buffer.add_string buf ";\n";
    | Exit document -> 
        addTabs();
        Buffer.add_string buf "exit "; 
        prettyDocument document;
        Buffer.add_string buf ";\n";
    | ReturnVoid -> 
        addTabs();
        Buffer.add_string buf "return ";
        Buffer.add_string buf ";\n";
    | ReturnExp exp -> 
        addTabs();
        Buffer.add_string buf "return ";
        prettyExp exp; 
        Buffer.add_string buf ";\n";
    | StmIfElse (exp, stm1, stm2) -> 
        addTabs();
        Buffer.add_string buf "if ( "; 
        prettyExp exp; 
        Buffer.add_string buf " )\n"; 
        prettyStm stm1; 
        if (stm2!=EmptyStm) then (
            addTabs(); 
            Buffer.add_string buf "else\n"; 
            prettyStm stm2
        )
    | StmWhile (exp, stm) ->
        addTabs();
        Buffer.add_string buf "while (";
        prettyExp exp; 
        Buffer.add_string buf ")\n"; 
        prettyStm stm;
    | CompStm compoundstm -> 
        prettyCompoundstm compoundstm;
    | StmExp exp -> 
        addTabs();
        prettyExp exp;
        Buffer.add_string buf ";\n";

  and prettyReceive receive = match receive with
    | Receive inputs -> 
        Buffer.add_string buf "receive[";
        prettyInputs inputs;
        Buffer.add_string buf "]";

  and prettyInputs inputs = match inputs with
    | [] -> 
        ()
    | [x] -> 
        prettyInput x;
    | x :: xs-> 
        prettyInput x; 
        Buffer.add_string buf ",";
        prettyInputs xs;

  and prettyInput input = match input with
    | Input (lvalue, identifier) ->
        prettyLvalue lvalue; 
        Buffer.add_string buf"=";
        prettyIdentifier identifier

  and prettyLvalue lvalue = match lvalue with
    | Lval (identifier) -> 
        prettyIdentifier identifier
    | LvalStruct (identifier1, identifier2) -> 
        prettyIdentifier identifier1;
        Buffer.add_string buf ".";
        prettyIdentifier identifier2

  and prettyDocument document = match document with
    | Document (identifier, []) -> 
        prettyIdentifier identifier;
        Buffer.add_string buf " "
    | Document (identifier, plugs) ->
        Buffer.add_string buf "plug ";
        prettyIdentifier identifier;
        Buffer.add_string buf "[";
        prettyPlugs plugs;
        Buffer.add_string buf "]";

  and prettyPlugs plugs = match plugs with
    | [] ->
        ()
    | [x] -> 
        prettyPlug x;
    | x :: xs -> 
        prettyPlug x;
        Buffer.add_string buf ",";
        prettyPlugs xs;

  and prettyPlug plug = match plug with
    | Plug (identifier, exp) ->
        prettyIdentifier identifier;
        Buffer.add_string buf "=";
        prettyExp exp;

  and prettyFunc func = match func with
    | Func (tp, identifier, arguments, compoundstm) ->
        addTabs();
        prettyTp tp;  
        prettyIdentifier identifier;
        Buffer.add_string buf "(";
        prettyArguments arguments; 
        Buffer.add_string buf ")\n";
        prettyCompoundstm compoundstm
                  
  and prettyFuncs funcs = match funcs with
    | [] -> 
        ()
    | x :: xs -> 
        prettyFunc x; 
        prettyFuncs xs

  and prettySessions sessions = match sessions with
    | [] ->
        ()
    | x :: xs -> 
        prettySession x;
        prettySessions xs

  and prettySession session = match session with
    | Session (identifier, compoundstm) ->
        addTabs();
        Buffer.add_string buf "session ";
        prettyIdentifier identifier;
        Buffer.add_string buf "()\n";
        prettyCompoundstm compoundstm

  in match s with
    | Service (htmls, schemas, variables, funcs, sessions) ->
        Buffer.clear buf;
        Buffer.add_string buf "service { \n"; 
        tabUp();
        prettyHtmls htmls; 
        prettySchemas schemas; 
        prettyVariables variables; 
        prettyFuncs funcs; 
        prettySessions sessions; 
        Buffer.add_string buf "}\n";
        tabDown()
