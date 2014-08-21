open Syntax

exception FuncNoReturn of identifier
exception SessNoExit of identifier
exception FuncNotFound of identifier

let weed s = 

  let Service (htmls, schemas, variables, funcs, sessions) = s in
  let lastId = ref (IdentifierConst ".") in

(* ------------------------------------NON-VOID FUNCTIONS RETURNING-------------------------------------------------*)

  let rec weedFuncs funcs = match funcs with
    | [] ->
        true
    | x :: xs ->
        weedFunc x && weedFuncs xs

  and weedFunc func = match func with
    | Func(VoidType, _, _, _) ->
        true
    | Func(tp, id, args, cstm) ->
        lastId := id;
        weedCompoundstmFromFun cstm;

  and weedCompoundstmFromFun cstm = match cstm with
    | Compound (_, stms) ->
        weedStmsFromFun stms;

  and weedStmsFromFun stms = match stms with
    | [] ->
        false
    | [x] ->
        weedStmFromFun x
    | x :: xs ->
        weedStmsFromFun xs

  and weedStmFromFun stm =  match stm with
    | ReturnExp exp ->
        true
    | StmIfElse (exp, stm1, stm2) ->
        weedStmFromFun stm1 && weedStmFromFun stm2
    | CompStm cstm ->
        weedCompoundstmFromFun cstm
    | _ ->
        false

(* --------------------------------------------SESSIONS EXITING-----------------------------------------------------*)

  and weedSessions sessions = match sessions with
    | [] ->
        true
    | x :: xs ->
        weedSessions xs && weedSession x

  and weedSession session = match session with
    | Session (id, cstm) ->
        lastId := id;
        weedCompoundstmFromSession cstm

  and weedCompoundstmFromSession cstm = match cstm with
    | Compound (_, stms) ->
        weedStmsFromSession stms

  and weedStmsFromSession stms = match stms with
    | [] ->
        false
    | x :: [] ->
        weedStmFromSession x
    | x :: xs ->
        weedStmsFromSession xs

  and weedStmFromSession stm =  match stm with
    | Exit document ->
        true
    | Show (document, receive) ->
        true
    | StmIfElse (exp, stm1, stm2) ->
        weedStmFromSession stm1 && weedStmFromSession stm2
    | CompStm cstm ->
        weedCompoundstmFromSession cstm
    | StmExp (FunctionExp (id, _)) ->
        findFuncExits id
    | _ ->
        false

(* --------------------------------------------------HELPER----------------------------------------------------------*)

  and findFuncExits id =
    let rec findFromFuncs functions = match functions with
      | [] ->
          raise (FuncNotFound id)
      | Func(tp,fid,args,cstm) :: xs ->
          if (Semantic.sameId (id,fid)) then (weedCompoundstmFromSession cstm) else findFromFuncs xs
    in
    findFromFuncs funcs

(* ------------------------------------------------MAIN CALL---------------------------------------------------------*)

  in
  if (weedFuncs funcs) then () else raise (FuncNoReturn !lastId);
  if (weedSessions sessions) then () else raise (SessNoExit !lastId)
;;




