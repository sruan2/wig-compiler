open Syntax
open Semantic

exception InvalidPlugType of identifier
exception InvalidReceiveType of identifier

let typecheck ast tree =

  let stack = Stack.create () in

  let rec checkFuncs funcs = match funcs with
    | [] -> 
        funcs
    | x :: xs ->
        let typedFunc = checkFunc x in typedFunc :: (checkFuncs xs)

  and checkFunc func = match func with
    | Func (tp, id, args, cstms) ->
        let _ = push() in
        let typedcstms = checkCstms tp cstms in
        let _ = pop() in
        Func(tp, id, args, typedcstms)

  and checkCstms tp cstms = match cstms with
    | Compound (vars, stms) ->
        let _ = push () in
        let typedstms = checkStms tp stms in
        let _ = pop () in 
        Compound(vars, typedstms)

  and checkStms tp stms = match stms with
    | [] -> 
        stms
    | x :: xs ->
        let typedstm = checkStm tp x in
        typedstm :: (checkStms tp xs)

  and checkStm tp stm = match stm with
    | EmptyStm -> 
        EmptyStm
    | Show (document, receive) -> 
        Show(checkDocument document, checkReceive receive)
    | Exit document -> 
        Exit (checkDocument document)
    | ReturnVoid -> 
        let expectedTp = tpToIdType tp in
        if Voidtp == expectedTp then ReturnVoid 
        else raise (StmTypeError (stm, "Return Void", Voidtp, expectedTp))
    | ReturnExp exp -> 
        let expectedTp = tpToIdType tp in
        let typedExp = checkExp exp in
        if sameType(expectedTp, typeof typedExp) then ReturnExp(typedExp)
        else raise (StmTypeError (stm, "Return Exp", typeof typedExp, expectedTp))
    | StmIfElse (exp, stm1, stm2) ->
        let returnedExp = checkExp exp in 
        let typedStm1' = checkStm tp stm1 in
        let typedStm2' = checkStm tp stm2 in
        if Booltp == typeof returnedExp then StmIfElse (returnedExp, typedStm1', typedStm2')
        else raise (StmTypeError (stm, "If-Else", typeof returnedExp, Booltp))
    | StmWhile (exp, stm') ->     
        let returnedExp = checkExp exp in 
        let typedStm = checkStm tp stm' in
        if Booltp == typeof returnedExp then StmWhile (returnedExp, typedStm)
        else raise (StmTypeError (stm, "While", typeof returnedExp, Booltp))
    | CompStm cstm -> 
        CompStm (checkCstms tp cstm)
    | StmExp exp -> 
        StmExp (checkExp exp)

  and checkDocument document = match document with
    | Document (id, plugs) -> 
        let idtp = lookuptp HtmlSymbol id in
        if idtp == Htmltp then Document(id, checkPlugs plugs)
        else raise (DocumentTypeError(document, "Document", idtp, Htmltp))

  and checkPlugs plugs = match plugs with
    | [] -> 
        plugs
    | x :: xs -> 
        checkPlug x :: checkPlugs xs

  and checkPlug plug = 
    let Plug(id,exp) = plug in
    let typedExp = checkExp exp in
    match typeof typedExp with
      | Stringtp | Inttp | Booltp -> Plug(id, typedExp)
      | _ -> raise (InvalidPlugType id)
			    
  and checkReceive receive = match receive with
    | Receive inputs -> Receive(checkInputs inputs)

  and checkInputs inputs = match inputs with
    | [] -> 
        inputs
    | x :: xs -> 
        checkInput x :: checkInputs xs

  and checkInput input = 
    let Input(lvalue, id) = input in 
    match typeof (checkLval lvalue) with
      | Stringtp | Inttp -> input 
      | _ -> raise (InvalidReceiveType id)

  and checkExp exp = match exp with
    | LValue lvalue -> 
        let typedLval = checkLval lvalue in
        TypedExp(typedLval, typeof typedLval)
    | LValueAssign (lvalue, exp') -> 
        let typedExp = checkExp exp' in
        (match (typeof (checkLval lvalue), typeof typedExp) with
          | (tp1, tp2) -> 
              if sameType(tp1,tp2) then TypedExp(LValueAssign(lvalue, typedExp), tp1)
              else raise (TypeError (exp, "=",tp2,tp1)))
    | Iseq(exp1, exp2)-> 
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (tp1, tp2) -> 
              if sameType(tp1,tp2) then TypedExp(Iseq(typed1,typed2), Booltp)
              else raise (TypeError (exp, "==",tp2,tp1)))
    | Neq (exp1, exp2) -> 
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (tp1, tp2) -> 
              if sameType(tp1,tp2) then TypedExp(Neq(typed1,typed2), Booltp)
              else raise (TypeError (exp, "!=",tp2,tp1)))
    | Le (exp1, exp2) ->      
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) -> 
              TypedExp(Le(typed1,typed2), Booltp)
          | (Inttp, tp2) -> 
              raise (TypeError (exp,"<",tp2, Inttp))
          | (tp1,tp2) -> 
              raise (TypeError (exp,"<",tp1, Inttp)))
    | Ge (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) -> 
              TypedExp(Ge(typed1,typed2), Booltp)
          | (Inttp, tp2) -> 
              raise (TypeError (exp,">",tp2, Inttp))
          | (tp1,tp2) -> 
              raise (TypeError (exp,">",tp1, Inttp)))
    | Leq (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) -> 
              TypedExp(Leq(typed1,typed2), Booltp)
          | (Inttp, tp2) -> 
              raise (TypeError (exp,"<=",tp2, Inttp))
          | (tp1,tp2) -> 
              raise (TypeError (exp,"<=",tp1, Inttp)))
    | Geq (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) -> 
              TypedExp(Geq(typed1,typed2), Booltp)
          | (Inttp, tp2) -> 
              raise (TypeError (exp,">=",tp2, Inttp))
          | (tp1,tp2) -> 
              raise (TypeError (exp,">=",tp1, Inttp)))
    | Bang exp' ->
        let typed = checkExp exp' in
        (match typeof typed with
          | Booltp -> TypedExp (Bang(typed), Booltp)
          | tp'    -> raise (TypeError (exp, "!", tp', Booltp)))
    | Uminus exp' ->
        let typed = checkExp exp' in
        (match typeof typed with
          | Inttp -> TypedExp (Uminus(typed), Inttp)
          | tp'    -> raise (TypeError (exp, "uminus-", tp', Inttp)))
    | Plus (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Stringtp, Stringtp) ->  TypedExp(Plus(typed1,typed2), Stringtp)
          | (Inttp, Inttp) ->  TypedExp(Plus(typed1,typed2), Inttp)
          | (Stringtp, Inttp) ->  TypedExp(Plus(typed1,typed2), Stringtp)
          | (Inttp, tp2) -> raise (TypeError (exp, "+", tp2, Inttp))
          | (Stringtp, tp2) ->  raise (TypeError (exp, "+", tp2, Stringtp))
          | (tp1, tp2) -> raise (TypeError (exp, "+", tp1, Inttp))) 
    | Minus (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) ->  TypedExp(Minus(typed1,typed2), Inttp)
          | (Inttp, tp2) -> raise (TypeError (exp, "-", tp2, Inttp))
          | (tp1, tp2) -> raise (TypeError (exp, "-", tp1, Inttp))) 
    | Mult (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) ->  TypedExp(Mult(typed1,typed2), Inttp)
          | (Inttp, tp2) -> raise (TypeError (exp, "*", tp2, Inttp))
          | (tp1, tp2) -> raise (TypeError (exp, "*", tp1, Inttp))) 
    | Div (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) ->  TypedExp(Div(typed1,typed2), Inttp)
          | (Inttp, tp2) -> raise (TypeError (exp, "/", tp2, Inttp))
          | (tp1, tp2) -> raise (TypeError (exp, "/", tp1, Inttp))) 
    | Mod (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Inttp, Inttp) ->  TypedExp(Mod(typed1,typed2), Inttp)
          | (Inttp, tp2) -> raise (TypeError (exp, "%", tp2, Inttp))
          | (tp1, tp2) -> raise (TypeError (exp, "%", tp1, Inttp))) 
    | And (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Booltp, Booltp) -> TypedExp(And(typed1,typed2), Booltp)
          | (Booltp, tp2) -> raise (TypeError (exp, "&&", tp2, Booltp))
          | (tp1, tp2) -> raise (TypeError (exp, "&&", tp1, Booltp)))  
    | Or (exp1, exp2) ->
        let typed1 = checkExp exp1 in
        let typed2 = checkExp exp2 in
        (match (typeof typed1, typeof typed2) with
          | (Booltp, Booltp) -> TypedExp(Or(typed1,typed2), Booltp)
          | (Booltp, tp2) -> raise (TypeError (exp, "||", tp2, Booltp))
          | (tp1, tp2) -> raise (TypeError (exp, "||", tp1, Booltp)))
    | Assign (exp1, exp2) ->
        let typedexp1 = checkExp exp1 in
        let typedexp2 = checkExp exp2 in
        let tp1 = typeof typedexp1 in
        let tp2 = typeof typedexp2 in
        (match (tp1, tp2) with
          | (Tupletp l1, Tupletp l2) -> 
              let rec checkList receiverTuple givingTuple = (match givingTuple with
                | [] -> receiverTuple
                | (givingId, givingTp)::tl -> 
                    try 
                      let typeInReceiver = List.assoc givingId receiverTuple in
                      if (sameType (givingTp,typeInReceiver)) then (checkList receiverTuple tl) 
                      else raise Not_found
                    with 
                        Not_found -> (checkList (receiverTuple @ [List.hd givingTuple]) tl))
              in
              TypedExp(Assign(typedexp1, typedexp2), Tupletp (checkList l1 l2))
          | (_,_) -> raise (TypeError (exp, "<<", tp2,tp1)))

        
    | BplusId (exp1, ids1) ->
        let typedexp1 = checkExp exp1 in
        (*let _ = print_endline ("type is "^ (idtypeToString (typeof typedexp1))) in*)
        (match (typeof typedexp1) with
          | Tupletp idNidtpList ->
              let rec f ids =
              (match ids with
                 | [] -> 
                     []
                 | x :: xs -> 
                     (try 
                         (List.find (fun (a,b) -> 
                          (* Question: is it sufficient that we only check the identifiers ? *)
                          (let n = String.compare (idToString a) (idToString x) in
                            (*print_endline "Comparison =============   ";
                            print_int n;*)
                           n == 0)) idNidtpList) :: (f xs)
                     with Not_found -> raise TupleBPlusError)
              ) in
              let newidNidtpList = f ids1 in
              TypedExp(BplusId(typedexp1,ids1), Tupletp newidNidtpList)
          | _ -> raise TupleBPlusError
        )

      | BminusId (exp1, ids1) ->
        let typedexp1 = checkExp exp1 in
        (*let _ = print_endline ("type is "^ (idtypeToString (typeof typedexp1))) in*)
        (match (typeof typedexp1) with
          | Tupletp idNidtpList ->
              let rec f ids =
              (match ids with
                 | [] -> 
                     []
                 | x :: xs -> 
                     (try 
                         (List.find (fun (a,b) -> 
                          (* Question: is it sufficient that we only check the identifiers ? *)
                          (let n = String.compare (idToString a) (idToString x) in
                            (*print_endline "Comparison =============   ";
                            print_int n;*)
                           n == 0)) idNidtpList) :: (f xs)
                     with Not_found -> raise TupleBminusError)
              ) in
              let newidNidtpList = f ids1 in
              let minusnewidNidtpList = List.filter (fun a -> ((List.mem a newidNidtpList) == false)) idNidtpList in
              TypedExp(BminusId(typedexp1,ids1), Tupletp minusnewidNidtpList)
          | _ -> raise TupleBminusError
        )  

        (*
        let expc = checkExp exp1 in
        let rec somefuncname e =
          (match e with | BplusId(exp',[id]) ->
          let typedExp = checkExp exp' in 
          let tp = typeof typedExp in
          (match tp with
            | Tupletp l1 -> 
                (try
                   let idtp = List.assoc id l1 in
                   TypedExp(typedExp, Tupletp ([(id, idtp)]))
                 with
                   Not_found -> raise (TupleBIdError ("\\+", tp, id)))
            | _ -> raise (TypeError (exp, "\\+",tp, (Tupletp [])))))
        in
        (match ids with
         | [] -> 
            TypedExp(BplusId(expc,ids),Tupletp [])
         | x :: xs -> 
            let Tupletp tupleattr = typeof (somefuncname (BplusId (exp1,[x]))) in
            let Tupletp tupleattrs =  typeof (checkExp (BplusId (exp1,xs))) in
            TypedExp(BplusId(expc,ids), Tupletp (tupleattr @ tupleattrs)))*)

    (*| BminusId (exp1, ids) ->
        let resultExp = checkExp exp1 in
        let resultType = typeof resultExp in
        let rec removeAttr listoftypes listofids = match listofids with
          | [] -> 
              listoftypes
          | x :: xs ->
              let updated = (List.filter (fun (eachid, eachtp) -> not (sameId(eachid, x))) listoftypes)
              in removeAttr updated xs
        in
        let removeResult = match resultType with
          | Tupletp tplist -> removeAttr tplist ids
          | othertype -> raise (TypeError (exp, "\\-", resultType, Tupletp []))
        in
        TypedExp(BminusId(exp,ids), Tupletp removeResult)*)

    | FunctionExp (id, exps) ->
        let tp = lookuptp FuncSymbol id in
        (match tp with
         | Functp (arglist, returntp) -> 
             let rec matchArgumentsWithExps alist elist = 
                (match (alist, elist) with 
                  | ([], []) -> returntp
                  | ((_,argtp)::tl, x :: xs) -> 
                      let exptp = typeof (checkExp x) in
                      if sameType(exptp, argtp) then matchArgumentsWithExps tl xs
                      else raise (TypeError (x, "function application "^idToString(id), exptp, argtp))
                  | (_, _) -> raise (FuncTypeError id))
             in let _ = matchArgumentsWithExps arglist exps in 
             TypedExp(exp, returntp)
         | _ -> raise (FuncTypeError id))
    | IntExp intconst ->
        TypedExp(exp, Inttp)
    | BoolExp boolean ->
        TypedExp(exp, Booltp)
    | StringExp stringconst ->
        TypedExp(exp, Stringtp)
    | Tuple fieldvalues ->
        let rec checkFields fieldvalues = (match fieldvalues with
          | [] -> 
              []
          | FieldValue (id,exp) :: xs -> 
              (id,typeof (checkExp exp)) :: (checkFields xs))
        in
        TypedExp(exp, Tupletp (checkFields fieldvalues))
    | ParenExp exp' ->
        let typedexp = checkExp exp' in
        TypedExp(ParenExp(typedexp), typeof typedexp)
    | TypedExp (exp',_) ->
        exp

  and checkLval lvalue = 
    match lvalue with
      | Lval id -> TypedExp(LValue lvalue, lookuptp VarSymbol id)
      | LvalStruct (id1,id2) -> TypedExp(LValue lvalue, lookuptpSchemaMember id1 id2)

  and checkSessions sessions =
    match sessions with
      | [] -> 
          sessions
      | x :: xs -> 
          let typedsession = checkSession x in
          let typedsessions = checkSessions xs in
          typedsession :: typedsessions

  and checkSession session =
    match session with
      | Session (identifier, compoundstm) -> 
              let _ = push() in
              let typedcstm = checkCstms VoidType compoundstm in
              let _ = pop() in
              Session(identifier, typedcstm)

  and tpToIdType = function
    | TupleType i -> 
        lookuptp SchemaSymbol i 
    | IntType -> Inttp
    | BoolType -> Booltp
    | StringType -> Stringtp
    | VoidType -> Voidtp

  and push () =
    let Node (_, children) = Stack.top stack in
    match !children with
    | c :: cs ->
        let _ = children := cs in
        Stack.push c stack
    | [] ->
        raise SymbolTableFailed

  and pop () =
    let Node (_, children) = Stack.top stack in
    match !children with
    | c :: cs ->
        raise SymbolTableFailed
    | [] ->
        Stack.pop stack

  and lookuptpSchemaMember schemaid memberid =
    try
      let f = fun (Node (ht, _)) ->
        try
          let Symbol (sym, data, _) = Hashtbl.find ht schemaid in
          if sym = VarSymbol then raise (FoundSchema (getHash data)) else ()
        with
          Not_found -> ()
      in
      let _ = Stack.iter f stack in
      raise (MissingDeclaration schemaid)
    with
      FoundSchema ht -> 
        try
          let Symbol (_, _, tp) = Hashtbl.find ht memberid in
          tp
        with
          Not_found -> raise (SchemaMemberMissing memberid)

  and getHash data = match data with
      | HashRef ht -> ht
      | _ -> raise MissingHashRef

  and lookuptp kind id =
    try
      let f = fun (Node (ht, _)) ->
        try
          let Symbol (sym, _, idtp) = Hashtbl.find ht id in
          if sym=kind then raise (FoundType idtp) else ()
        with
          Not_found -> ()
      in
      let _ = Stack.iter f stack in
      raise (MissingDeclaration id)
    with
      FoundType idtp -> idtp

  in
  match ast with Service (htmls, schs, vars, funcs, sess) ->
      Stack.push tree stack;
      let typedfuncs = checkFuncs funcs in
      let typedsess = checkSessions sess in
      Service(htmls, schs, vars, typedfuncs, typedsess)
