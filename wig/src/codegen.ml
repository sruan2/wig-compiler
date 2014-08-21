open Printf
open String
open Syntax
open Semantic

(* the show document occurs in the wrong scope*)
exception DocumentWrongPosition of document * identifier
exception TupleAssignError of exp
exception BminusError
exception BAssignError

let generate file ast tree =

  let stack = Stack.create () in

  let _ = Stack.push tree stack in

  (* keep a reference to the global hashtable *)
  let globalhbl = let Node (ht, _) = Stack.top stack in 
                  ref ht in

  (* the accumulator to record the number of stages in a session *)
  let acc = ref 0 in

  (* remove the path part and suffix *) 
  let convert_name s = 
     try
       let m = String.length s in
       let n = String.rindex s '/' in 
       (sub s (n+1) (m-n-5))
     with Not_found -> 
       print_endline "not find /";sub s 0 ((length s) - 4) 
  in

  (* the following will be printed at the beginning of the generated c file *)
  let message = 
  "/******************************************
#     wig compiler-A by:
#
#    David Thibodeau
#    Ioannis Fytilis
#    Sherry Shanshan Ruan
#  
#    copyright @ group-A 2013
#
 ******************************************/\n" in

  let lib = 
  "/* Include library functions */\n
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>\n
#include \"wigA_run.c\"\n" in

  let definebool = 
  "/* Booleans are represented as ints */\n
#define true 1
#define false 0\n
typedef int bool;\n" in

  let globalvar = 
  "/* Define global variables */\n
char * url;
char * sessionid;
char gb_name[] = \""^(convert_name file) ^".gb\";\n
FILE *gf;
FILE *lf;\n" in

  let schemasection =
  "/* Wig schema section */\n" in

  let varsection = 
  "/* Wig global variable section */\n" in

  let htmlsection =
  "/* Wig html section */\n" in

  let sessionsection = 
  "/* Wig session section */\n" in

  let globalfunsection =
  "/* Wig global function declaration */\n" in

  let funcsection = 
  "/* Wig function section */\n" in

  let maininit = 
"/* Main function section */\n
int main (int argc, char *agrv[])
{\n\t/* Initialize random seed */
\tsrand48(time((time_t *)0));\n
\t/* Parse fields */
\tparseFields();
\turl = catString(\"http://\", catString(getenv(\"SERVER_NAME\"), getenv(\"SCRIPT_NAME\")));
\tsessionid = getenv(\"QUERY_STRING\");\n
\t/* Decide which service to launch */
\tif (sessionid == NULL) goto error_and_exit;\n" in

  let mainend1 = 
"/* Display the error page if an invalid sessionid is passed */
error_and_exit:
\tprintf(\"Content-type: text/html\\n\\n\");
\tprintf(\"<html><body>\\n\");
\tprintf(\"<h3>Sorry, your session has expired or is not valid.</h3>\\n\");
\tprintf(\"<h3>Valid session(s) are:</h3>\\n\");\n" in

  let mainend2 = 
"\tprintf(\"</body></html>\\n\");
\texit(1);
}\n" in

  let rec genHtmls oc htmls = match htmls with
    | [] -> 
        ()
    | x :: xs -> 
        genHtml oc x;
        genHtmls oc xs

  and genHtml oc html =
    let Html (identifier, htmlbodies) = html in
    (* to obtain a list containg all arguments the html page contains*)
    let taglist = findHtmltaglist identifier in
    (*let f = fun a b l -> 
      (match b with
        | Symbol(BodyTagSymbol,_,Nonetp) -> 
            let id = idToSmartStr a in id::l
        | _ -> 
            l ) 
    in
    let arglist = Hashtbl.fold f ht [] in
    let revarglist = List.sort arglist in*)
    let _ = fprintf oc "void html_%s(char *url, char *sessionid" (idToSmartStr identifier) in
    let rec printtaglist alist = 
      (match alist with
        | [] -> 
            ()
        | x :: xs -> 
            fprintf oc ", char *%s" (idToSmartStr x);
            printtaglist xs )
    in 
    printtaglist taglist;
    fprintf oc ")\n{\n";
    fprintf oc "\tprintf(\"Content-type: text/html\\n\\n\");\n";
    fprintf oc "\tprintf(\"<html>\\n\") ;\n";
    fprintf oc "\tprintf(\"<form method=\\\"POST\\\" action=\\\"%s?%s\\\">\\n\", url, sessionid);\n" "%s" "%s";
    genHtmlbodies oc htmlbodies;
    fprintf oc "\tprintf(\"<br><input type=\\\"submit\\\" value=\\\"Continue\\\">\\n\");\n";
    fprintf oc "\tprintf(\"</form><html>\\n\");\n";
    fprintf oc "}\n\n";

  and genHtmlbodies oc htmlbodies = match htmlbodies with
    | [] ->
        ()
    | x :: xs ->
        genHtmlbody oc x;
        genHtmlbodies oc xs

  and genHtmlbody oc htmlbody = match htmlbody with
    | TagAttr (identifier, attributes) -> 
        let id = idToSmartStr identifier in
        fprintf oc "\tprintf(\"<%s" id; (* space is printed in genAttributes*)
        genAttributes oc attributes;
        fprintf oc ">\\n\");\n";
    | TagClosed (identifier) -> 
        let id = idToSmartStr identifier in
        fprintf oc "\tprintf(\"</%s" id;
        fprintf oc ">\\n\");\n";
    | TagBracked (identifier) -> 
        let id = idToSmartStr identifier in
        fprintf oc "\tprintf(\"%s\", %s" "%s" id;
        fprintf oc ");\n";
    | Whatever (WhateverConst s) -> 
        fprintf oc "\tprintf(\"%s\");\n" (escaped s);
    | Meta (MetaConst s) -> 
        fprintf oc "printf(\"<!--%s-->\");\n" (escaped s);
    | TagInput inputattrs -> 
        fprintf oc "\tprintf(\"<input"; (* space is printed in genInputAttrs*)
        genInputAttrs oc inputattrs;
        fprintf oc ">\");\n";
    | TagSelect (inputattrs, htmlbodies) -> 
        fprintf oc "\tprintf(\"<select"; (* space is printed in genInputAttrs*)
        genInputAttrs oc inputattrs;
        fprintf oc ">\");";
        genHtmlbodies oc htmlbodies;
        fprintf oc "\tprintf(\"</select>\");\n";
        
  and genAttributes oc attributes = match attributes with
    | [] ->
        ()
    | x :: xs ->
        fprintf oc " ";
        genAttribute oc x;
        genAttributes oc xs
          
  and genAttribute oc attribute = match attribute with
    | Attr attr ->                              
        genAttr oc attr     
    (* Remove " and " around numbers for size attribute i.e. size = 5 instead of size = "5" *)                            
    | PairAttr (AttrID (IdentifierConst ("size")), AttrString (StringConst n)) ->
        fprintf oc "size=";
        let s2 = sub n 1 (length n -2) in
        fprintf oc "%s" s2
    | PairAttr (attr1, attr2) -> 
        genAttr oc attr1;
        fprintf oc "=";
        genAttr oc attr2; 
        
  and genAttr oc attr = match attr with
    | AttrID identifier ->
        let id = idToSmartStr identifier in
        fprintf oc "%s" id
    | AttrString stringconst ->
        genSmartStringConst oc stringconst   
    | AttrBident identifier ->
        (* for debugging *)
        (*fprintf oc "/*[bident]*/";*)
        fprintf oc "\");\n";
        let id = idToSmartStr identifier in
        fprintf oc "\tprintf(\"%s\", %s" "%s" id;
        fprintf oc ");\n";
        fprintf oc "\tprintf(\""

  and genInputAttrs oc inputattrs = match inputattrs with
    | [] ->
        ()
    | x :: xs ->
        fprintf oc " ";      (* space for separating inputattrs*)
        genInputAttr oc x;
        genInputAttrs oc xs
          
  and genInputAttr oc inputattr = match inputattr with
    | InputAttr attr ->
        fprintf oc "name=";
        genAttr oc attr;
    | InputType inputtype ->
        fprintf oc "type=";
        let genInputType inputtype = 
        (match inputtype with
          | Text -> fprintf oc "\\\"text\\\""
          | Radio -> fprintf oc "\\\"radio\\\"")
        in genInputType inputtype
    | Attribute attribute ->
        genAttribute oc attribute
                                              
  and genSchemas oc schemas = match schemas with
    | [] ->
        ()
    | x :: xs ->
        genSchema oc x;
        genSchemas oc xs;
        
  and genSchema oc schema = 
    let Schema (identifier, fields) = schema in
    (*let ht = findSchemaHash identifier in
      let f = fun a b l -> 
      (match b with
      | Symbol(BodyTagSymbol,_,Nonetp) -> let IdentifierConst id = a in id::l
      | _ -> l) in
      let arglist = Hashtbl.fold f ht [] in
      let _ = fprintf oc "void html_%s(char *url, char *sessionid" (idToString identifier) in
      let rec printarglist arglist = (match arglist with
      | [] -> ()
      | x :: xs -> fprintf oc ", char *%s" x;
      printarglist xs)
      in printarglist arglist;*)
    fprintf oc "typedef struct {\n";
    genFields oc fields;
    fprintf oc "} %s;\n\n" (idToSmartStr identifier);
    
  and genFields oc fields = match fields with
    | [] ->
        ()
    | x :: xs ->
        genField oc x;
        genFields oc xs;
  and genField oc field = 
    let Field (tp, identifier) = field in
    fprintf oc "\t";
    genTp oc tp;
    if tp = StringType then fprintf oc " *%s;\n" (idToSmartStr identifier)
    else fprintf oc " %s;\n" (idToSmartStr identifier);

  and genVariables oc variables n= match variables with
    | [] ->
        ()
    | x :: xs ->
        genVariable oc x n;
        genVariables oc xs n;
  and genVariable oc variable n = 
    let Var (tp, identifiers) = variable in
    print_tabs oc n;
    genTp oc tp;
    fprintf oc " ";
    genVarIdentifiers oc identifiers n;
    fprintf oc ";\n"

  and genTp oc tp = match tp with
    | IntType ->
        fprintf oc "int"
    | BoolType ->
        fprintf oc "bool" 
    | StringType ->
        fprintf oc "char"
    | VoidType ->
        fprintf oc "void" 
    | TupleType identifier ->
        fprintf oc "%s" (idToSmartStr identifier); 
                                                
  (* global function declartion at the beginning of the c file *)
  and genFuncstitle oc funcs = match funcs with
    | [] ->
        ()
    | x :: xs ->
        genFunctitle oc x;
        genFuncstitle oc xs;

  and genFunctitle oc func = 
    let Func (tp, identifier, arguments, _) = func in
    genTp oc tp;
    (* genTp only prints char, we need to make it a pointer explicitly here*)
    if tp = StringType then fprintf oc "*"; 
    fprintf oc " func_%s(" (idToSmartStr identifier);
    genArguments oc arguments;
    fprintf oc ");\n";

  and genFuncs oc funcs = match funcs with
    | [] ->
        ()
    | x :: xs ->
        genFunc oc x;
        genFuncs oc xs;
        
  and genFunc oc func = 
    let Func (tp, identifier, arguments, compoundstm) = func in
    let _ = push() in
    let _ = genTp oc tp in
    let _ = if tp = StringType then fprintf oc "*" in
    let _ = fprintf oc " func_%s(" (idToSmartStr identifier) in
    let _ = genArguments oc arguments in
    let _ = fprintf oc ")\n{\n"in
    let _ = genCompoundstm oc compoundstm identifier 1 in (*1 stands for 1 tag*)
    let _ = pop() in
    fprintf oc "}\n\n"

  and genArguments oc arguments = match arguments with
    | [] ->
        ()
    | x :: [] ->
        genArgument oc x
    | x :: xs ->
        genArgument oc x;
        fprintf oc ", ";
        genArguments oc xs;

  and genArgument oc argument = 
    let Argument (tp, identifier) = argument in
    genTp oc tp;
    if tp = StringType then fprintf oc " *%s" (idToSmartStr identifier)
    else fprintf oc " %s" (idToSmartStr identifier);

  and genSessions oc sessions = match sessions with
    | [] ->                                     
        ()
    | x :: xs ->
        genSession oc x ;
        genSessions oc xs 

  and genSession oc session = 
    let Session(identifier, compoundstm) = session in
    let name = idToSmartStr identifier in
    let _ = fprintf oc "void session_%s(int stage)\n{\n" name in
    let _ = push() in  
    (* we need to pass session hash table to compoundstm 
       because we need to read and write local variables 
       at the beginning of if and while *)
    let _ = genCompoundstm oc compoundstm identifier 1 in
    let _ = pop() in
    fprintf oc "}\n\n";

  and genCompoundstm oc compoundstm id n = 
    let Compound (variables, stms) = compoundstm in
    let _ = push() in
    let _ = genVariables oc variables n in
    let _ = if variables != [] then fprintf oc "\n" else () in

    (* if it's a session, then we need to print the following: *)
    let _ = (if getSymboltp id = SessionSymbol then
    let _ = initializeTuple oc variables n in
    (* print the stage control flow first *)
    (let localstage = getStageNum id in
    let _ = fprintf oc "\t/* Control flow for different stages */\n" in
    let _ = fprintf oc "\tint step = 0;\n" in
    let _ = acc := 0 in (* we need to print the stage control flow from 0 to localstage*)
    let _ = fprintf oc "\tif (stage == 0) goto step_%d;\n\n" (!acc) in
    let _ = fprintf oc "\t/* Get the stage where we branch from the file */\n" in
    let _ = fprintf oc "\tlf = fopen(sessionid, \"r\");\n" in
    let _ = fprintf oc "\tstep = fgetc(lf);\n" in
    let _ = fprintf oc "\tfclose(lf);\n" in
    let _ = acc := !acc +1 in
    while (!acc) < !localstage do 
      fprintf oc "\tif (step == %d) goto step_%d;\n" (!acc) (!acc);
      acc := (!acc) + 1
    done ;
    fprintf oc "\texit(1);\n\n";
    (* immediately, we print the first step (step 0) in this session*)
    acc := 0; (* we need to print the stage label from 0 to localstage, so reset acc = 0 *)
    fprintf oc "/* step_%d : first time into this session */\n" (!acc);
    fprintf oc "step_%d:\n\n" (!acc);
    fprintf oc "\t/* Create a new sessionid for the connection to store locals and globals */\n";
    fprintf oc "\tsessionid = randomString(\"%s\", 20);\n\n" (idToSmartStr id);
    (* if exist global varialbes, then fread all global variables *)
    step0GlobalVars oc n))

    in
    let _ = genStms oc stms id n in
    let _ = pop() in
    ()
   


  and initializeTuple oc variables n = match variables with
    | [] -> 
        fprintf oc "\n";
    | Var((TupleType tupleid),identifiers) :: xs ->
        (match lookuptp SchemaSymbol tupleid with
          | Tupletp idNidtpList ->
              let rec f oc l anIdentifer =
              (match l with
                | [] -> ()
                | (xid,xidtp)::xs -> 
                  if xidtp = Stringtp then
                  (print_tabs oc n;
                  fprintf oc "copyString(&(%s.%s), \"\");\n" (idToSmartStr anIdentifer) (idToSmartStr xid););
                  f oc xs anIdentifer;
              ) in
              let _ = (match identifiers with
              | [] -> ()
              | anId::ids -> f oc idNidtpList anId)
              in
               initializeTuple oc xs n;
          | _ -> ()
          )
    | _ :: xs ->
        initializeTuple oc xs n;    

  and genStms oc stms id n = match stms with
    | [] ->
        ()
    | x :: xs ->
        genStm oc x id n;
        genStms oc xs id n
          
  (* To be done: format to print pretty tags for different scopes*)
  and genStm oc stm id n = match stm with
    | EmptyStm -> 
        ()
    | Show (document, receive) -> 
        print_tabs oc n;
        genShowDocument oc document id n;
        genReceive oc receive n;  
    | Exit document -> 
        print_tabs oc n;
        genExitDocument oc document id n;  
    | ReturnVoid -> 
        ()
    | ReturnExp exp ->              
        print_tabs oc n;
        fprintf oc "return ";
        genExp oc exp n; 
        fprintf oc ";\n"
    | StmIfElse (exp, stm1, stm2) ->
        fprintf oc "\n";
        print_tabs oc n;
        fprintf oc "if(";
        genExp oc exp n;
        fprintf oc ")\n";
        print_tabs oc n;
        fprintf oc "{";
        genStm oc stm1 id (n+1);
        print_tabs oc n;
        fprintf oc "}\n";
        if stm2 != EmptyStm then
          (print_tabs oc n;
           fprintf oc "else\n";
           print_tabs oc n;
           fprintf oc "{";
           genStm oc stm2 id (n+1);
           print_tabs oc n;
           fprintf oc "}\n")         
    | StmWhile (exp, stm') ->     
        fprintf oc "\n";
        print_tabs oc n;
        fprintf oc "while(";
        genExp oc exp n;
        fprintf oc ")\n";
        print_tabs oc n;
        fprintf oc "{";
        genStm oc stm' id (n+1);
        print_tabs oc n;
        fprintf oc "}\n"
    | CompStm (Compound (vars', stms)) -> 
        let _ = push() in
        let _ = genVariables oc vars' n in
        let _ = fprintf oc "\n" in
        let _ = genStms oc stms id n in
        let _ = pop() in
        ()
    | StmExp exp -> 
        print_tabs oc n;
        genExp oc exp n;
        fprintf oc ";\n"
          
  (* Receive corresponds to getField *)
  and genReceive oc receive n = match receive with
    | Receive inputs ->
        genInputs oc inputs n
          
  and genInputs oc inputs n = match inputs with
    | [] ->
        ()
    | x :: xs ->
        genInput oc x n;
        genInputs oc xs n;
        
  and lvalueToString lvalue = match lvalue with
    | Lval id -> 
        idToSmartStr id
    | LvalStruct (id1, id2)->
        (idToSmartStr id1)^"."^(idToSmartStr id2) 
                                
  and genInput oc input n = 
    let Input (lvalue, identifier) = input in
    (match lvalue with
      | LvalStruct (id1,id2) -> 
          () 
      | Lval id -> 
          let tp = lookuptp VarSymbol id in
          (match tp with
            (* not sure about stringtp *)
            | Stringtp -> print_tabs oc n;
                fprintf oc "copyString(&%s, getField(\"%s\"));\n" (lvalueToString lvalue) (idToSmartStr identifier) (*order?*)
            | Inttp -> print_tabs oc n;
                fprintf oc "%s = atoi(getField(\"%s\"));\n" (lvalueToString lvalue) (idToSmartStr identifier)
            | _ -> 
                fprintf oc "receive other type\n"
          )
    )
  

  and genShowDocument oc document id n = 
    let Document (identifier, plugs) = document in
    (match (getSymboltp id) with
      | SessionSymbol -> 
          (* store local variables*)
          let _ = fwriteLocalVars oc (!acc) n in
          (* store global variables*)
          let _ = fwriteGlobalVars oc n in
          (* finally, display the html page and exit(0)*)
          genPlugs oc identifier plugs n;
          if (!acc) < !(getStageNum id) -1 then (* won't create new stages if reach the localstage *)
          (acc := !acc +1;
          fprintf oc "step_%d:\n\n" (!acc); (* Start the next stage *)
          freadLocalVars oc n;
          freadGlobalVars oc n;)
          else ();
      (* if it's a function *)
      | FuncSymbol -> 
          genPlugsForFun oc identifier plugs n;
      | _ -> 
          raise (DocumentWrongPosition (document,id))
    )
    
  and genExitDocument oc document id n = 
    let Document (identifier, plugs) = document in
    (match (getSymboltp id) with
      | SessionSymbol -> 
          (* store local variables*)
          let _ = fwriteLocalVars oc (!acc-1) n in
          (* store global variables*)
          let _ = fwriteGlobalVars oc n in
          (* remove session id before exit *)
          print_tabs oc n;
          fprintf oc "/* We remove session id before exiting */\n";
          print_tabs oc n;
          fprintf oc "remove(sessionid);\n";
          print_tabs oc n;
          fprintf oc "sessionid = \"\";\n\n";
          (* finally, display the html page and exit(0)*)
          genPlugs oc identifier plugs n;
          if (!acc) < !(getStageNum id) - 1 then (* stop creating new stages if reach the localstage *)
            (acc := !acc +1;
             fprintf oc "step_%d:\n\n" (!acc); (* Start the next stage *)
             freadLocalVars oc n;
             freadGlobalVars oc n)  (*At the beginning of each stage, we get locals and globals from temp file before doing anything*)
    else ();
      (* if it's a function *)
      | FuncSymbol -> 
          genPlugsForFun oc identifier plugs n;
      | _ -> 
          raise (DocumentWrongPosition (document,id))
    )
    
    

  and genExp oc exp n = match exp with
    | LValue lvalue -> 
        genLValue oc lvalue
    | LValueAssign (lvalue, exp') -> 
        (match (typeof exp') with
          | Inttp ->
             genLValue oc lvalue;
             fprintf oc " = ";
             genExp oc exp' n
          | Stringtp ->
             fprintf oc "copyString(&";
             genLValue oc lvalue;
             fprintf oc ",";
             genExp oc exp' n;
             fprintf oc ")"
          | Booltp ->
             genLValue oc lvalue;
             fprintf oc " = ";
             genExp oc exp' n
          | Tupletp _ -> 
             let rec f exp = 
             (match exp with   
               | Tuple fieldvalues -> 
                   (match lvalue with 
                     | Lval tupleid -> genFieldvalues oc tupleid fieldvalues n
                     (* for debugging *)
                     | LvalStruct (id1,id2) -> fprintf oc "LVALSTRUCT %s %s" (idToSmartStr id1) (idToSmartStr id2)
                   )
               | BplusId (exp,ids) ->
                   genBplusID oc lvalue exp ids n
               | BminusId (exp,ids) ->
                   (match (typeof exp') with
                    | Tupletp l -> genBminusID oc lvalue l exp ids n
                    | _ -> raise BminusError
                   )
               | Assign (exp1,exp2) ->
                   (match (typeof exp') with
                    | Tupletp l -> genAssign oc lvalue l exp1 exp2 n
                    | _ -> raise BAssignError
                   )          
               | TypedExp (exp2,_) -> f exp2
               | ParenExp exp2 -> f exp2
               | _ ->
                    genLValue oc lvalue;
                    fprintf oc " = ";
                    genExp oc exp' n;                 
             )
             in f exp'
          | _ -> 
             genLValue oc lvalue;
             fprintf oc " = ";
             genExp oc exp' n
        )
    | Iseq(exp1, exp2)-> 
        (match (typeof exp1) with
          | Inttp -> 
             fprintf oc "(";
             genExp oc exp1 n;
             fprintf oc "==";
             genExp oc exp2 n;
             fprintf oc ")";
          | Stringtp ->  
             fprintf oc "(strcmp(";
             genExp oc exp1 n;
             fprintf oc ",";
             genExp oc exp2 n;
             fprintf oc ")==0)"
          | Booltp -> 
             fprintf oc "(";
             genExp oc exp1 n;
             fprintf oc "==";
             genExp oc exp2 n;
             fprintf oc ")";
          | _ -> 
             ()
        )        
    | Neq (exp1, exp2) -> 
        (match (typeof exp1) with
          | Inttp -> 
             fprintf oc "(";
             genExp oc exp1 n;
             fprintf oc "!=";
             genExp oc exp2 n;
             fprintf oc ")";
          | Stringtp ->  
             fprintf oc "(strcmp(";
             genExp oc exp1 n;
             fprintf oc ",";
             genExp oc exp2 n;
             fprintf oc ")!=0)"
          | Booltp -> 
             fprintf oc "(";
             genExp oc exp1 n;
             fprintf oc "!=";
             genExp oc exp2 n;
             fprintf oc ")";
          | _ -> 
             ()
        )
    | Le (exp1, exp2) ->      
        genExp oc exp1 n;
        fprintf oc "<";
        genExp oc exp2 n
    | Ge (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc ">";
        genExp oc exp2 n;
        fprintf oc ")";
    | Leq (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc "<=";
        genExp oc exp2 n;
        fprintf oc ")";
    | Geq (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc ">=";
        genExp oc exp2 n;
        fprintf oc ")";
    | Bang exp' ->
        fprintf oc "(";
        fprintf oc "!";
        genExp oc exp' n;
        fprintf oc ")";
    | Uminus exp' ->
        fprintf oc "(";
        fprintf oc "-";
        genExp oc exp' n;
        fprintf oc ")";
    | Plus (exp1, exp2) ->
        (match (typeof exp1) with
          | Stringtp -> 
             fprintf oc "catString(";
             genExp oc exp1 n;
             fprintf oc ", ";
             genExp oc exp2 n;
             fprintf oc ")"  
          | _ -> 
             fprintf oc "(";
             genExp oc exp1 n;
             fprintf oc "+";
             genExp oc exp2 n;
             fprintf oc ")";
        )
    | Minus (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc "-";
        genExp oc exp2 n;
        fprintf oc ")";
    | Mult (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc "*";
        genExp oc exp2 n;
        fprintf oc ")";
    | Div (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc "/";
        genExp oc exp2 n;
        fprintf oc ")";
    | Mod (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc "%s" "%";
        genExp oc exp2 n;
        fprintf oc ")";
    | And (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc ")";
        fprintf oc " && ";
        fprintf oc "(";
        genExp oc exp2 n;
        fprintf oc ")";
    | Or (exp1, exp2) ->
        fprintf oc "(";
        genExp oc exp1 n;
        fprintf oc ")";
        fprintf oc " || ";
        fprintf oc "(";
        genExp oc exp2 n;
        fprintf oc ")";
    | Assign (exp1, exp2) ->
        (match (typeof exp1, typeof exp2) with
          | (Tupletp idNidtpList1, Tupletp idNidtpList2) ->
              let rec f l = 
              (match l with
                | [] -> ()
                | (xid,xidtp) :: xs -> 
                  (match (List.mem (xid,xidtp) idNidtpList2) with
                    | true -> ()
                    | false -> genExp oc exp2 n
                  );
                  f xs
              ) in
              f idNidtpList1
          | (_,_) -> raise BAssignError)
    | BplusId (exp1, ids) ->
        fprintf oc "ENTER BPLUSID";
        fprintf oc "bplus exp1 is %s, type is %s, id is %s, type is %s" (expToString exp1) (idtypeToString (typeof exp1)) (idToSmartStr (List.hd ids)) (idtypeToString (lookuptp VarSymbol (List.hd ids)));
        genExp oc exp1 n
    | BminusId (exp1, ids) ->
        fprintf oc "ENTER BMINUS";
        genExp oc exp1 n
    | FunctionExp (id, exps) ->
        fprintf oc "func_%s(" (idToSmartStr id);
        genExps oc exps n;
        fprintf oc ")"
    | IntExp intconst ->
        fprintf oc "%d" intconst
    | BoolExp boolean ->
        fprintf oc "%s" (string_of_bool boolean) (* To be done: need to convert to 0,1*)
    | StringExp stringconst ->
        let StringConst str = stringconst in
        fprintf oc "%s" str
    | Tuple fieldvalues ->
        ()
    | ParenExp exp' ->
        fprintf oc "(";
        genExp oc exp' n;
        fprintf oc ")";
    | TypedExp (exp',_) ->
        genExp oc exp' n

  and  genAssign oc lvalue idNidtpList exp1 exp2 n = match (typeof exp1, typeof exp2) with
    | (Tupletp l1, Tupletp l2) -> (match idNidtpList with
         |  [] -> ()
         | (xid,xidtp)::[] -> if (List.mem (xid,xidtp) l2) then
                              (
                               genLValue oc lvalue;
                               fprintf oc ".%s = " (idToSmartStr xid);
                               genExp oc exp2 n;
                               fprintf oc ".%s" (idToSmartStr xid);
                              )
                              else
                              (
                               genLValue oc lvalue;
                               fprintf oc ".%s = " (idToSmartStr xid);
                               genExp oc exp1 n;
                               fprintf oc ".%s" (idToSmartStr xid);
                              )

        | (xid,xidtp)::xs -> if (List.mem (xid,xidtp) l2) then
                              (
                               genLValue oc lvalue;
                               fprintf oc ".%s = " (idToSmartStr xid);
                               genExp oc exp2 n;
                               fprintf oc ".%s;\n" (idToSmartStr xid);
                              )
                              else
                              (
                               genLValue oc lvalue;
                               fprintf oc ".%s = " (idToSmartStr xid);
                               genExp oc exp1 n;
                               fprintf oc ".%s;\n" (idToSmartStr xid);
                              );
                            print_tabs oc n;
                            genAssign oc lvalue xs exp1 exp2 n;
                            )
    | (_,_) -> raise BAssignError;

  and  genBplusID oc lvalue exp ids n = match ids with
    | [] ->
        ()
    | x :: [] ->
        genLValue oc lvalue;
        fprintf oc ".%s = " (idToSmartStr x);
        genExp oc exp n;
        fprintf oc ".%s" (idToSmartStr x);
    | x :: xs ->    
        genLValue oc lvalue;
        fprintf oc ".%s = " (idToSmartStr x);
        genExp oc exp n;
        fprintf oc ".%s;\n" (idToSmartStr x);
        print_tabs oc n;
        genBplusID oc lvalue exp xs n;

  and  genBminusID oc lvalue idNidtpList exp ids n = match idNidtpList with
    |  [] -> ()
    | (xid,xidtp)::[] -> if ((List.mem xid ids) == false) then
                            (
                            genLValue oc lvalue;
                            fprintf oc ".%s = " (idToSmartStr xid);
                            genExp oc exp n;
                            fprintf oc ".%s" (idToSmartStr xid);
                            )
    | (xid,xidtp)::xs -> if ((List.mem xid ids) == false) then
                            (
                            genLValue oc lvalue;
                            fprintf oc ".%s = " (idToSmartStr xid);
                            genExp oc exp n;
                            fprintf oc ".%s;\n" (idToSmartStr xid);
                            print_tabs oc n;
                            );
                        genBminusID oc lvalue xs exp ids n;
     

  and genFieldvalues oc tupleid fieldvalues n = match fieldvalues with
    | [] ->
        ()
    | x :: [] ->
        genFieldvalue oc tupleid x n;
    | x :: xs ->
        genFieldvalue oc tupleid x n;
        fprintf oc ";\n";
        print_tabs oc n;
        genFieldvalues oc tupleid xs n;

  and genFieldvalue oc tupleid fieldvalue n = 
    let FieldValue(id, exp) = fieldvalue in
    fprintf oc "%s.%s = " (idToSmartStr tupleid) (idToSmartStr id);
    genExp oc exp n;
    
          
  and genLValue oc lvalue = match lvalue with
    | Lval identifier ->
        fprintf oc "%s" (idToSmartStr identifier) 
    | LvalStruct (identifier1, identifier2) ->
        fprintf oc "%s.%s" (idToSmartStr identifier1) (idToSmartStr identifier2)
          
  and genExps oc exps n = match exps with
    | [] ->
        ()
    | x :: [] ->
        genExp oc x n;
    | x :: xs ->
        genExp oc x n;
        fprintf oc ", ";
        genExps oc xs n

  (* this function is defined on identifers for variables *)
  and genVarIdentifier oc identifier n = 
    let id = idToSmartStr identifier in 
    (match lookuptp VarSymbol identifier with
      | Inttp ->  fprintf oc "%s" id
      | Stringtp -> fprintf oc "*%s = \"\"" id
      | Tupletp l -> fprintf oc "%s" id;
                     (*let rec f list = (match list with
                       | [] -> ()
                       | (xid,xidtp)::[] -> 
                         if xidtp = Stringtp then
                         (fprintf oc ";\n";
                          print_tabs oc n;
                         fprintf oc "copyString(&(%s.%s),\"\")" id (idToSmartStr xid))
                       | (xid,xidtp)::xs -> 
                         if xidtp = Stringtp then
                         (fprintf oc ";\n";
                          print_tabs oc n;
                         fprintf oc "copyString(&(%s.%s),\"\");\n" id (idToSmartStr xid));
                         f xs)
                      in f l*)


      | _ -> fprintf oc "%s" id
    )  

  and genVarIdentifiers oc identifiers n = match identifiers with
    | [] ->
        ()
    | x :: [] ->
        genVarIdentifier oc x n; (* don't put any comma after the last variable *)
    | x :: xs ->
        genVarIdentifier oc x n;
        fprintf oc ", ";
        genVarIdentifiers oc xs n

  (* modified strings to be compatiable with html attributes *)
  (* so this function can only be called by html attributes *)
  and genSmartStringConst oc stringconst =
    let StringConst s = stringconst in 
    let substr = sub s 1 ((length s) - 2 ) in (* remove the beginning " and ending " *)
    (* if it ends with %, we need to change it to %% as c recoginizes %% as %*)
    let substr2 = strToSmartStr substr "" in
    let substr3 = "\\\"" ^ substr2 ^ "\\\"" in
    fprintf oc "%s" substr3

  (* generate the main c function*)
  and genMain oc sessions = 
    let _ = fprintf oc "%s\n" maininit in
    let f = (fun a -> 
            let Session(identifier,_) = a in
            let id = idToSmartStr identifier in
            fprintf oc "\tif (strcmp(sessionid,\"%s\") == 0) session_%s(0);\n\tif (strncmp(sessionid,\"%s$\",%d) == 0) session_%s(1);\n\n" id id id ((length id)+1) id) 
    in
    let g = (fun a -> 
            let Session(identifier,_) = a in
            let id = idToSmartStr identifier in
            fprintf oc "\tprintf(\"   --> %s\");\n\tprintf(\"<br>Example: <b><i>%s?%s</i></b><br><br>\\n\", getenv(\"SCRIPT_NAME\"));\n" id "%s" id) 
    in
    List.iter f sessions;
    fprintf oc "%s" mainend1;
    List.iter g sessions;
    fprintf oc "%s" mainend2

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

  and getSymboltp id =  
    try
      let f = fun (Node (ht, _)) ->
        try
          let Symbol (sym, _, _) = Hashtbl.find ht id in
          raise (FoundSymboltp sym)
        with
          Not_found -> ()
      in
      let _ = Stack.iter f stack in
      raise (MissingDeclaration id)
    with
      FoundSymboltp sym -> sym     

  and gettaglist data = match data with
      | HtmlData (_,l) -> l
      | _ -> raise MissingHtmlTaglist   

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


  and findHtmltaglist id =
    try
      let f = fun (Node (ht, _)) ->
        try
          let Symbol (sym, data, _) = Hashtbl.find ht id in
          if sym = HtmlSymbol then raise (FoundHtmltaglist (gettaglist data)) else ()
        with
          Not_found -> ()
      in
      let _ = Stack.iter f stack in
      raise (MissingDeclaration id)
    with
      FoundHtmltaglist l -> l    

  (* collect all local variables: we collect variables in each hashtable
     from the top of the stack to the one just above the bottom hashtable:
     because we don't need global variables, we don't go inside global hashtable *)
  and collectSessVars unit =
    let varht = Hashtbl.create 20 in
    let n = ref (Stack.length stack) in
    let f = fun (Node (ht, _)) ->
      n := !n - 1;
      if !n >1 then
        (let g id sym = 
           try 
              let _ = Hashtbl.find varht id in
              ()
           with Not_found -> 
                  let Symbol(symtp,_,_ ) = sym in
                  (match symtp with
                      | VarSymbol -> Hashtbl.add varht id sym
                      | _ -> ()
                  )
        in Hashtbl.iter g ht;())
      else ()
    in
    Stack.iter f stack;
    varht

  (* get the number of documents (stages) a session contains *)
  and getStageNum id =
    try
      let f = fun (Node (ht, _)) ->
        try 
          let Symbol (sym, data, _) = Hashtbl.find ht id in
          if sym = SessionSymbol then (match data with
          | StageNum x -> raise (FoundStage x)
          | _ -> ()) 
          else ()
        with
          Not_found -> ()
      in
      let _ = Stack.iter f stack in
      raise (MissingDeclaration id)
    with
      FoundStage s -> s
          
  (*and top unit =
    let Node (ht, children) = Stack.top stack in
    ht  *)

  (* To fread all local varialbes in the current session *)
  and freadLocalVars oc n = 
    let f a b = let Symbol (sym, _, idtp ) = b in
                if sym = VarSymbol then freadlfHelp oc n idtp (idToSmartStr a)
    in 
    print_tabs oc n;
    fprintf oc "/* Read local variables from temp file */\n";
    print_tabs oc n;
    fprintf oc "lf = fopen(sessionid, \"r\");\n";
    print_tabs oc n;
    fprintf oc "fgetc(lf);\n";
    Hashtbl.iter f (collectSessVars ()) ;
    print_tabs oc n;
    fprintf oc "fclose(lf);\n\n"

  (* To fread all global varialbes in the current session *)
  and freadGlobalVars oc n = 
    let f a b = let Symbol (sym, _, idtp ) = b in
                if sym = VarSymbol then freadgfHelp oc n idtp (idToSmartStr a)
    in 
    print_tabs oc n;
    fprintf oc "/* Read global variables from temp file */\n";
    print_tabs oc n;
    fprintf oc "gf = fopen(gb_name, \"r\");\n";
    Hashtbl.iter f (!globalhbl);
    print_tabs oc n;
    fprintf oc "fclose(gf);\n\n"

  (* To fwrite all local varialbes in the current session *)
  (* id is the session identifier because we need to retrieve information from session hashtable *)
  and fwriteLocalVars oc i n = 
    let f a b = let Symbol (sym, _, idtp ) = b in
                if sym = VarSymbol then fwritelfHelp oc i n idtp (idToSmartStr a)
                else fprintf oc "not varSymbol\n"
    in 
    fprintf oc "\n";
    print_tabs oc n;
    fprintf oc "/* Write local variables into temp file */\n";
    print_tabs oc n;
    fprintf oc "lf = fopen(sessionid, \"w\");\n";
    print_tabs oc n;
    fprintf oc "fputc(%d, lf);\n" (i+1);
    Hashtbl.iter f (collectSessVars ()) ;
    print_tabs oc n;
    fprintf oc "fclose(lf);\n\n"  

  (* To fwrite all global varialbes in the current session *)
  and fwriteGlobalVars oc n = 
    let f a b = let Symbol (sym, _, idtp ) = b in
                if sym = VarSymbol then fwritegfHelp oc n idtp (idToSmartStr a)
    in 
    print_tabs oc n;
    fprintf oc "/* Write global variables into temp file */\n";
    print_tabs oc n;
    fprintf oc "gf = fopen(gb_name, \"w\");\n";
    Hashtbl.iter f (!globalhbl);
    print_tabs oc n;
    fprintf oc "fclose(gf);\n\n"  
  

  (* To print fread global variables inside step_0 (only for step_0!) in each session*)
  and step0GlobalVars oc n =
    let f a b = let Symbol (sym, _, idtp ) = b in
                if sym = VarSymbol then freadgfHelp oc (n+1) idtp (idToSmartStr a)
    in
    print_tabs oc n; 
    fprintf oc "/* Read all global variables first */\n";
    print_tabs oc n;
    fprintf oc "gf = fopen(gb_name, \"r\");\n";
    print_tabs oc n;
    fprintf oc "if (gf != NULL)\n";
    print_tabs oc n;
    fprintf oc "{\n";
    Hashtbl.iter f (!globalhbl);
    print_tabs oc n;
    fprintf oc "\tfclose(gf);\n";
    print_tabs oc n;
    fprintf oc "}\n\n"

  and fwritelfHelp oc i n idtp idname = match idtp with
    | Inttp ->    
        print_tabs oc n;
        fprintf oc "fwrite(&%s, sizeof(int), 1, lf);\n" idname
    | Stringtp -> print_tabs oc n;
        fprintf oc "{\n";
        print_tabs oc n;
        fprintf oc "\tint tmpi;\n";
        print_tabs oc n;
        fprintf oc "\ttmpi = strlen(%s);\n" idname;
        print_tabs oc n;
        fprintf oc "\tfwrite(&tmpi, sizeof(int), 1, lf);\n";
        print_tabs oc n;
        fprintf oc "\tfwrite(%s, sizeof(char), strlen(%s), lf);\n" idname idname;
        print_tabs oc n;
        fprintf oc "}\n"
    | Booltp ->   
        print_tabs oc n;
        fprintf oc "fwrite(&%s, sizeof(int), 1, lf);\n" idname
    | Tupletp l ->
        (match l with
          | []  ->  
              ()
          | (xidname, xidtp) :: xs ->  
              fwritelfHelp oc i n xidtp (idname^"."^(idToSmartStr xidname));
              fwritelfHelp oc i n (Tupletp xs) idname;
        )
    | _ -> 
        fprintf oc "Error: other type\n"  
          
  and fwritegfHelp oc n idtp idname = match idtp with
    | Inttp ->    
        print_tabs oc n;
        fprintf oc "fwrite(&%s, sizeof(int), 1, gf);\n" idname
    | Stringtp -> 
        print_tabs oc n;
        fprintf oc "{\n";
        print_tabs oc n;
        fprintf oc "\tint tmpi;\n";
        print_tabs oc n;
        fprintf oc "\ttmpi = strlen(%s);\n" idname;
        print_tabs oc n;
        fprintf oc "\tfwrite(&tmpi, sizeof(int), 1, gf);\n";
        print_tabs oc n;
        fprintf oc "\tfwrite(%s, sizeof(char), strlen(%s), gf);\n" idname idname;
        print_tabs oc n;
        fprintf oc "}\n"
    | Booltp ->                                      
        print_tabs oc n;
        fprintf oc "fwrite(&%s, sizeof(int), 1, gf);\n" idname
    | Tupletp l ->
        (match l with
          | []  ->  
               ()
          | (xidname, xidtp) :: xs ->  
               fwritegfHelp oc n xidtp (idname^"."^(idToSmartStr xidname));
               fwritegfHelp oc n (Tupletp xs) idname;
        )
    | _ -> 
        ()
          
  and freadlfHelp oc n idtp idname = match idtp with
    | Inttp -> 
        print_tabs oc n;
        fprintf oc "fread(&%s, sizeof(int), 1, lf);\n" idname
    | Stringtp -> 
        print_tabs oc n;
        fprintf oc "{\n";
        print_tabs oc n;
        fprintf oc "\tint tmpi;\n";
        print_tabs oc n;
        fprintf oc "\tfread(&tmpi, sizeof(int), 1, lf);\n";
        print_tabs oc n;
        fprintf oc "\t%s = (char *)malloc(tmpi+1);\n" idname;
        print_tabs oc n;
        fprintf oc "\tfread(%s, sizeof(char), tmpi, lf);\n" idname;
        print_tabs oc n;
        fprintf oc "\t%s[tmpi] = '\\0';\n" idname;
        print_tabs oc n;
        fprintf oc "}\n"
    | Booltp ->   
        print_tabs oc n;
        fprintf oc "fread(&%s, sizeof(int), 1, lf);\n" idname
    | Tupletp l ->
        (match l with
          | []  ->  
              ()
          | (xidname, xidtp) :: xs ->  
              freadlfHelp oc n xidtp (idname^"."^(idToSmartStr xidname));
              freadlfHelp oc n (Tupletp xs) idname;
        )                                                  
    | _ -> 
        ()                                                   
          
  and freadgfHelp oc n idtp idname = match idtp with
    | Inttp ->                                        
        print_tabs oc n;
        fprintf oc "fread(&%s, sizeof(int), 1, gf);\n" idname
    | Stringtp ->                                     
        print_tabs oc n;
        fprintf oc "{\n";
        print_tabs oc n;
        fprintf oc "\tint tmpi;\n";
        print_tabs oc n;
        fprintf oc "\tfread(&tmpi, sizeof(int), 1, gf);\n";
        print_tabs oc n;
        fprintf oc "\t%s = (char *)malloc(tmpi+1) ;\n" idname;
        print_tabs oc n;
        fprintf oc "\tfread(%s, sizeof(char), tmpi, gf);\n" idname;
        print_tabs oc n;
        fprintf oc "\t%s[tmpi] = '\\0';\n" idname;
        print_tabs oc n;
        fprintf oc "}\n"
    | Booltp ->                                       
        print_tabs oc n;
        fprintf oc "fread(&%s, sizeof(int), 1, gf);\n" idname           
    | Tupletp l->  
        (match l with
          | []  ->  
              ()
          | (xidname, xidtp) :: xs ->  
              freadgfHelp oc n xidtp (idname^"."^(idToSmartStr xidname));
              freadgfHelp oc n (Tupletp xs) idname;
        ) 
    | _ -> 
        ()
  (* if the document is called by a session *)        
  (* plugs are provided as arguments to a html function *)
  and genPlugs oc identifier plugs n =
    (* we may need a typed exp here*)
    let f a = 
    let Plug(id, exp) = a in
    (match (typeof exp) with
      | Inttp -> 
          fprintf oc ", itoa(";
          genExp oc exp n;
          fprintf oc ")"
      | Stringtp -> 
          fprintf oc ", ";
          genExp oc exp n;
      | Booltp -> 
          fprintf oc ", itoa(";
          genExp oc exp n;
          fprintf oc ")"
      | _ ->        
          fprintf oc ", ";
          genExp oc exp n;)
    in
    print_tabs oc n;
    fprintf oc "/* Call to display the html */\n";
    print_tabs oc n;
    fprintf oc "html_%s(url, sessionid" (idToSmartStr identifier);
    List.iter f plugs;
    fprintf oc ");\n";
    print_tabs oc n;
    fprintf oc "exit(0);\n\n";

  (* if the document is called by a function *)
  and genPlugsForFun oc identifier plugs n =
    (* we may need a typed exp here*)
    let f a = 
    let Plug(id, exp) = a in
    (match (typeof exp) with
      | Inttp -> 
          fprintf oc ", itoa(";
          genExp oc exp n;
          fprintf oc ")"
      | Stringtp -> 
          fprintf oc ", ";
          genExp oc exp n;
      | Booltp -> 
          fprintf oc ", itoa(";
          genExp oc exp n;
          fprintf oc ")"
      | _ ->        
          fprintf oc ", ";
          genExp oc exp n;)
    in
    fprintf oc "/* Call to display the html */\n";
    print_tabs oc n;
    fprintf oc "html_%s(url, sessionid" (idToSmartStr identifier);
    List.iter f plugs;
    fprintf oc ");\n";

  (*we need to concatenate a string based on +, to achieve this,
    we remove + sign and return a nested catString expression *)
  and catstr exp = match exp with
    | Plus(exp1, exp2) -> "catString("^(catstr exp1)^", "^(catstr exp2)^")"
    | TypedExp (exp,_) -> catstr exp
    | _ -> expToString exp


  and print_tabs oc n =
    let i = ref n in 
    while !i > 0 do (let _ = i:= !i - 1 in fprintf oc "\t" ) done;

  (* add _A suffix to identifiers to avoid reserved words *)
  and idToSmartStr id = 
    let s = idToString id in
    (match s with
      | "continue" -> "continue_A"
      | "url" -> "url_A"
      | "sessionid" -> "sessionid_A"
      | _ -> s
    )
   
  
  (*and strcstToSmartStr strcst =
    let StringConst s = strcst in
    strToSmartStr s ""*)

  (* replace % with %% to avoid reserved symbols in c*) 
  and strToSmartStr s prefix =
    try 
        let n = String.index s '%' in
        let m = String.length s in
        let newprefix = (String.sub s 0 (n+1))^"%" in
        strToSmartStr (String.sub s (n+1) (m-n-1)) (prefix^newprefix)
    with Not_found -> (prefix^s)
     
  in

  (************ The main code generation part ************)
  let Service (htmls, schemas, variables, funcs, sessions) = ast in

  (************ generate the c file ************)
  let cfile = (convert_name file)^"A.c" in
  let oc = open_out cfile in
  (* copyright message *)
  fprintf oc "%s\n" message;
  (* include libraries *)
  fprintf oc "%s\n" lib;
  (* define booleans*)
  fprintf oc "%s\n" definebool;
  (* declare global varialbes*)
  fprintf oc "%s\n" globalvar;

  (* generate code for shcemas *)
  if schemas != [] then
  (fprintf oc "%s\n" schemasection; 
  genSchemas oc schemas;)
  else ();

  (* wig gloabl function declaration *)
  if funcs != [] then
  (fprintf oc "%s\n" globalfunsection;
  genFuncstitle oc funcs;
  fprintf oc "\n")
  else ();

  (* generate code for wig global variables *)
  if variables != [] then
  (fprintf oc "%s\n" varsection;
  genVariables oc variables 0; (* the bool flag is for tag format*)
  fprintf oc "\n";)
  else ();

  (* generate code for htmls *)
  if htmls != [] then  
  (fprintf oc "%s\n" htmlsection;    
  genHtmls oc htmls;)
  else ();
  
  (* generate code for functions*)
  if funcs!= [] then
  (fprintf oc "%s\n" funcsection;
  genFuncs oc funcs;)
  else ();

  (* generate code for sessions *)
  if sessions != [] then
  (fprintf oc "%s\n" sessionsection;
  genSessions oc sessions;)
  else ();

  (* generate main function which acts as an interface among sessions*)
  genMain oc sessions;

  close_out oc;
  print_endline (cfile^" was generated");

  (************ generate an executable install file ***********)
  let installfile = (convert_name file)^"A.install" in
  let cgifile = (convert_name file)^"A.cgi" in
  let insoc = open_out installfile in
  fprintf insoc "#!/bin/sh\n";
  fprintf insoc "gcc -I$WIGDIR/lib -w %s -o ~/public_html/cgi-bin/%s\n" cfile cgifile;
  fprintf insoc "chmod 711 ~/public_html/cgi-bin/%s\n" cgifile;
  close_out insoc;
  let n = Sys.command ("chmod +x "^installfile) in
  if n = 0 then print_endline (installfile^" was generated") (* Not sure of return code here *)
  else print_endline ("return code is " ^ (string_of_int n));

  (************ Execute the install file automatically ***********)
   
  let n = Sys.command ("./"^installfile) in (* how to catch c compiler error? *)
  if n = 0 then 
    (print_endline (installfile^" was executed");
     print_endline "Congratulations! Your wig service is successfully installed\n";)
  else print_endline ("return code is " ^ (string_of_int n));
  
;;
