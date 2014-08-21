open Syntax

open Semantic

let htlength = 10
let buf = Buffer.create 16

let printBuffer unit =
  print_string (Buffer.contents buf)

let symbolTable serv =

  let stack = Stack.create () in

  let rec buildGlobalHtmls htmls = match htmls with
    | [] -> 
        ()
    | Html (id, bodies) :: xs ->
        Buffer.add_string buf ("\tENTER: Building Html Body: " ^ idToString id ^"\n");
        let ht = Hashtbl.create htlength in
        let taglist = buildBodies ht bodies [] in
        Buffer.add_string buf ("\tEXIT: Building Html Body: " ^ idToString id ^ " {\n");
        print ht 2;
        add id HtmlSymbol (HtmlData (ht,taglist)) Htmltp;
        Buffer.add_string buf "\t\n}";
        buildGlobalHtmls xs
      
  and buildSchemas schs = match schs with
    | [] -> 
        ()
    | Schema (id, fields) :: xs ->
        Buffer.add_string buf ("\tENTER: Building Schema: " ^ idToString id ^ "\n");
        let ht = Hashtbl.create htlength in
        let tuple = buildFields ht fields [] in
        Buffer.add_string buf ("\tEXIT: Building Schema: " ^ idToString id ^ " {\n");
        print ht 2;
        add id SchemaSymbol (HashRef ht) (Tupletp tuple);
        Buffer.add_string buf "\t\n}";
        buildSchemas xs

  and buildGlobalSessions sess = match sess with
    | [] -> 
        ()
    | Session (id, ctsm) :: xs ->
        add id SessionSymbol (StageNum (ref (countPlugs ctsm))) Sessiontp;
        buildGlobalSessions xs

  and buildVars vars = match vars with
    | [] -> ()
    | Var (tp, ids) :: vars' -> match ids with
        | [] -> 
            buildVars vars'
        | x :: xs ->
          let _ = match tp with 
            | TupleType tupleId -> 
                add x VarSymbol (HashRef (findSchemaHash tupleId)) (tpToIdType tp)
            | simpletype ->
                add x VarSymbol (IsDeclared false) (tpToIdType simpletype)
          in
          buildVars (Var (tp, xs) :: vars')

  and buildGlobalFuncs funcs = match funcs with
    | [] -> 
        ()
    | Func (tp, id, args, cstms) :: xs ->
        let args = gatherArgsTypes args [] in
        add id FuncSymbol NoData (Functp (args, tpToIdType tp));
        buildGlobalFuncs xs

  and gatherArgsTypes args acc = match args with
    | [] -> 
        acc
    | Argument (tp, id) :: xs ->
        gatherArgsTypes xs (acc @ [(id, tpToIdType tp)])

  and buildBodies ht bodies taglist = match bodies with
    | [] -> 
        List.rev taglist
    | x :: xs ->
        let l = buildBody ht x taglist in
        buildBodies ht xs l

  and buildBody ht body taglist = match body with
    | TagAttr (id, attrs) -> 
        buildAttributes ht attrs taglist
    | TagClosed id -> 
        taglist
    | TagBracked id ->
        addToHT ht id BodyTagSymbol NoData Nonetp false;
        (match (List.mem id taglist) with
          | true -> taglist
          | false -> id::taglist)
    | Whatever w -> 
        taglist
    | Meta m -> 
        taglist
    | TagInput ia ->
        buildInputattrs ht ia taglist;
    | TagSelect (ia, bodies) ->
        let l = buildInputattrs ht ia taglist in
        buildBodies ht bodies l

  and buildAttributes ht attributes taglist = match attributes with
    | [] ->
        taglist
    | x :: xs ->
        let l = buildAttribute ht x taglist in
        buildAttributes ht xs l

  and buildAttribute ht attribute taglist = match attribute with
    | Attr attr ->
        buildAttr ht attr taglist
    | PairAttr (attr1, attr2) ->
        let l = buildAttr ht attr1 taglist in
        buildAttr ht attr2 l
        
  and buildAttr ht attr taglist = match attr with
    | AttrBident id ->
        addToHT ht id BodyTagSymbol NoData Nonetp false;
         (match (List.mem id taglist) with
          | true -> taglist
          | false -> id::taglist)
    | _ -> taglist

  and buildInputattrs ht iattrs taglist = match iattrs with
    | [] -> 
        taglist
    | InputAttr (AttrString (StringConst str)) :: xs ->
        let stripped = String.sub str 1 (String.length str - 2) in
        let id = IdentifierConst stripped in
        let _ = addToHT ht id BodyVarSymbol NoData Nonetp false in
        buildInputattrs ht xs taglist
    | Attribute attr :: xs ->
        let l = buildAttribute ht attr taglist in
        buildInputattrs ht xs l
    | _ :: xs ->
        buildInputattrs ht xs taglist

  and buildFields ht fields acc = match fields with
    | [] -> 
        acc
    | Field (stp, id) :: xs ->
        let tp = tpToIdType stp in
        addToHT ht id FieldSymbol NoData tp true;
        buildFields ht xs (acc @ [(id, tp)])

  and buildFuncs funcs = match funcs with
    | [] -> 
        ()
    | Func (tp, id, args, cstms) :: xs ->
        Buffer.add_string buf ("\tENTER: Building Functions "^idToString id ^ "\n");
        push();
        buildArgs args;
        buildCstms cstms 2;
        Buffer.add_string buf ("\tEXIT: Building Function "^idToString id^" {\n");
        pop() 2;
        Buffer.add_string buf "\t\n}";
        buildFuncs xs

  and buildSessions sess = match sess with
    | [] -> 
        ()
    | Session(id, cstms) :: xs ->
        Buffer.add_string buf ("\tENTER: Building Session: "^idToString id ^ "\n");
        push();
        buildCstms cstms 2;
        Buffer.add_string buf ("\tEXIT: Building Session: "^idToString id ^" {\n");
        pop() 2;
        Buffer.add_string buf "\t\n}";
        buildSessions xs

  and buildArgs args = match args with
    | [] -> 
        ()
    | Argument (tp, id) :: xs -> 
        let _ = match tp with 
          | TupleType tupleId ->
              add id VarSymbol (HashRef (findSchemaHash tupleId)) (tpToIdType tp)
          | simpletype ->
              add id VarSymbol (IsDeclared true) (tpToIdType simpletype)
        in
        buildArgs xs

  and buildCstms cstms n = match cstms with
    | Compound (vars, stms) ->
        print_tabs n;
        Buffer.add_string buf "ENTER: Compound Statement\n";
        push();
        buildVars vars;
        buildStms stms n;
        print_tabs n;
        Buffer.add_string buf "EXIT: Compound Statement {\n";
        pop() (n+1);
        print_tabs n;
        Buffer.add_string buf "}\n"

  and buildStms stms n = match stms with
    | [] -> 
        ()
    | x :: xs ->
        buildStm x n;
        buildStms xs n

  and buildStm stm n = match stm with
    | EmptyStm -> 
        ()
    | Show (doc, re) ->
        buildReceive (buildDocument doc) re
    | Exit doc ->
        let _ = buildDocument doc in
        ()
    | ReturnVoid -> ()
    | ReturnExp exp ->
        buildExp exp
    | StmIfElse (exp, stm', stm'') ->
        buildExp exp;
        buildStm stm' (n+1);
        buildStm stm'' (n+1)
    | StmWhile (exp, stm') ->
        buildExp exp;
        buildStm stm' (n+1)
    | CompStm cstm ->
        buildCstms cstm n
    | StmExp exp ->
        buildExp exp

  and buildExps exps = match exps with
    | [] -> 
        ()
    | x :: xs ->
        buildExp x;
        buildExps xs

  and buildExp exp = match exp with
    | LValue lval ->
        buildLval lval
    | LValueAssign (lval, e) ->
        buildExp e;
        buildLval lval
    | Iseq (e1,e2)
    | Neq (e1,e2)
    | Le (e1,e2)
    | Ge (e1,e2)
    | Leq (e1,e2)
    | Geq (e1,e2)       
    | Plus (e1,e2)
    | Minus (e1,e2)
    | Mult (e1,e2)
    | Div (e1,e2)
    | Mod (e1,e2)
    | And (e1,e2)
    | Or (e1,e2)
    | Assign (e1,e2) ->
        buildExp e1;
        buildExp e2
    | Bang e
    | Uminus e ->
        buildExp e
    | BplusId (e,ids) -> ()
    | BminusId (e,ids) -> ()
    | FunctionExp (id, exps) ->
        buildExps exps;
        let _ = lookup FuncSymbol id in
        ()
    | IntExp i -> ()
    | BoolExp b  -> ()
    | StringExp str -> ()
    | Tuple fields -> ()
    | ParenExp exp ->
        buildExp exp
    | TypedExp (exp, id) -> 
        buildExp exp

  and buildReceive ht receive = match receive with
    | Receive inputs -> buildInputs ht inputs

  and buildDocument doc = match doc with
    | Document (id, plugs) ->
        let _ = lookup HtmlSymbol id in
        let ht = findHtmlHash id in
        buildPlugs ht plugs;
        ht

  and buildInputs ht inputs = match inputs with
    | [] -> 
        ()
    | Input (lval,id) :: xs ->
        buildInputs ht xs;
        let _ =
          try Hashtbl.find ht id
          with Not_found -> raise MissingBodyVar
          in
        ()

  and buildPlugs ht plugs = match plugs with
    | [] -> 
        ()
    | Plug (id, exp) :: xs ->
        buildExp exp;
        buildPlugs ht xs

  and buildLval lval = match lval with
    | Lval id ->
        let _ = lookup VarSymbol id in ()
    | LvalStruct (id1,id2) ->
        let _ = lookup VarSymbol id1 in
        let _ = lookupSchemaMember id1 id2 in
        ()

  (* SIMPLE HELPER FUNCTIONS *)

  and getHash data = match data with
      | HtmlData (ht,_) -> ht
      | HashRef ht -> ht
      | _ -> raise MissingHashRef

  and findHtmlHash id =
    try
      let f = fun (Node (ht, _)) ->
        try
          let Symbol (sym, data, _) = Hashtbl.find ht id in
          if sym = HtmlSymbol then raise (FoundHtmlht (getHash data)) else ()
        with
          Not_found -> ()
      in
      Stack.iter f stack;
      raise (MissingDeclaration id)
    with
      FoundHtmlht ht -> ht

    and findSchemaHash id =
    try
      let f = fun (Node (ht, _)) ->
        try
          let Symbol (sym, data, _) = Hashtbl.find ht id in
          if sym = SchemaSymbol then raise (FoundSchema (getHash data)) else ()
        with
          Not_found -> ()
      in
      Stack.iter f stack;
      raise (MissingDeclaration id)
    with
      FoundSchema ht -> ht

  and lookupSchemaMember schemaid memberid =
    let schemasymbol = lookup VarSymbol schemaid in match schemasymbol with 
      | Symbol(sym,HashRef ht,tp) -> 
          (try
            let _ = Hashtbl.find ht memberid in ()
          with
          | Not_found -> raise (MissingDeclaration memberid))
      | _ -> raise (SchemaMemberMissing schemaid)

  and lookup kind id =
    try
      let f = fun (Node (ht, _)) ->
        try 
          let Symbol (sym, _, _) as s = Hashtbl.find ht id in
          if sym=kind then raise (FoundItem s) else ()
        with
          Not_found -> ()
      in
      Stack.iter f stack;
      raise (MissingDeclaration id)
    with
      FoundItem s -> s

  and pop unit n =
    let _ = popAll () n in ()

  and popAll unit i =
    let Node (ht, _) as n = Stack.pop stack in
    let _ = try
      let Node (_, children) = Stack.top stack in
      children := !children @ [n]
    with _ -> ()
    in
    let _ =  print ht i in n

  and push unit =
    let ht = Hashtbl.create htlength in
    Stack.push (Node (ht, (ref []))) stack

  and top unit =
    let Node (ht, children) = Stack.top stack in
    ht

  and add id kind data tp =
    addToHT (top ()) id kind data tp true

  and addToHT ht id kind data tp checkdup =
    try
      let _ = Hashtbl.find ht id in
      if checkdup then raise (Duplicate id) else ()
    with
      Not_found -> Hashtbl.add ht id (Symbol (kind,data,tp))

  and tpToIdType = function
    | TupleType i -> let Symbol (sym, data, tp) = lookup SchemaSymbol i in tp
    | IntType -> Inttp
    | BoolType -> Booltp
    | StringType -> Stringtp
    | VoidType -> Voidtp

  and idToString id =
        let IdentifierConst(id') = id in id'

  and print_tabs n =
      let i = ref n in while !i > 0 do 
      (let _ = i:= !i - 1 in Buffer.add_string buf "\t" ) done

  and print ht n =
    let symToString sym = match sym with
      | HtmlSymbol    -> "Html"
      | BodyTagSymbol -> "Dynamic binding"
      | BodyVarSymbol -> "Form Input"
      | SchemaSymbol  -> "Schema"
      | FieldSymbol   -> "Field"
      | VarSymbol     -> "Variable"
      | FuncSymbol    -> "Function"
      | SessionSymbol -> "Session"
    in
    Hashtbl.iter (fun id (Symbol (sym, data, idtp)) ->
      let _ = print_tabs n in
      Buffer.add_string buf ((idToString id)^" | of symbol type: "^(symToString sym) ^ " | of type: "^ idtypeToString idtp ^ "\n")) ht

  in
  match serv with Service (htmls, schs, vars, funcs, sess) ->
    Buffer.add_string buf "ENTER: Global\n";
    push();
    buildGlobalHtmls htmls;

    buildSchemas schs;
    buildVars vars;

    buildGlobalFuncs funcs;
    buildGlobalSessions sess;

    buildFuncs funcs;
    buildSessions sess;

    Buffer.add_string buf "EXIT: Global {\n";
    let tree = popAll () 1 in
    Buffer.add_string buf "}\n";
    tree
