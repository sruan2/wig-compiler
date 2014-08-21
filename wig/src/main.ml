open Syntax
open Semantic
open Lexing

exception DataFeedNotInitialized
exception SyntaxTreeNotInitialized

(* Options *)

let verbose = ref false
let pretty = ref false
let weed = ref false
let symbol = ref false
let typecheck = ref false
let generate = ref false
let file = ref "fileNotInitialized"
let ast = ref None

let set_file f = file := f

(* Data feed *)

let lineBuffer = ref None
let datafeed unit = match !lineBuffer with
  | Some x -> x
  | None -> raise DataFeedNotInitialized

(* Actions *)

let state = ref Lexer.BaseLevel
let lexing =
  (fun lexbuf -> match !state with
  | Lexer.BaseLevel -> Lexer.lex state lexbuf
  | Lexer.CommentLevel _ -> Lexer.comment state lexbuf
  | Lexer.HTMLLevel n -> Lexer.html n state lexbuf
  | Lexer.InputAttrLevel (n, b) -> Lexer.inputattr n b state lexbuf
  | Lexer.AttrIDLevel (n, b) -> Lexer.attrid n b state lexbuf
  )

let getAst unit = match !ast with
  | Some x -> x
  | None -> raise SyntaxTreeNotInitialized
let showPretty unit =
  Pretty.prettySyntax (getAst());
  Pretty.printBuffer ()
let performWeed unit =
  Weed.weed (getAst())
let showSymbols unit =
  let _ = Symbol.symbolTable (getAst()) in
  Symbol.printBuffer ()
let performTypecheck unit =
  ast := Some(Typecheck.typecheck (getAst()) (Symbol.symbolTable (getAst())))
let performGeneration unit =
  ast := Some(Typecheck.typecheck (getAst()) (Symbol.symbolTable (getAst())));
  Codegen.generate !file (getAst()) (Symbol.symbolTable (getAst()))

(* Main *)
let main =
begin

  let speclist = 
    [
    ("-v", Arg.Set verbose, "Enables verbose mode"); (* Future work? *)
    ("-p", Arg.Set pretty, "Displays a pretty code representation");
    ("-w", Arg.Set weed, "Enables weeding");
    ("-s", Arg.Set symbol, "Displays the symbol table");
    ("-t", Arg.Set typecheck, "Enables typechecking");
    ("-c", Arg.Set generate, "Generate a compiled C file");
    ("-f", Arg.String (set_file), "Selects a file to compile");
    ]
  in let usage_msg = "===========================================\nWiG Compiler. Do you need help? Here it is!"
  in Arg.parse speclist print_endline usage_msg;

  try
    let _ = lineBuffer := Some (Lexing.from_channel (open_in !file)) in
    let _ = ast := Some(Parser.parse lexing (datafeed())) in
    let _ = if !weed then let _ = performWeed() in () in
    let _ = if !symbol then let _ = showSymbols() in () in
    let _ = if !typecheck then let _ = performTypecheck() in () in
    let _ = if !pretty then let _ = showPretty() in () in
    let _ = if !generate then let _ = performGeneration() in () in
    ()
  with
  | Sys_error id ->
      Printf.eprintf "System error: %s%!\n" id

  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg

  | Parser.Error -> 
    let pos = Lexing.lexeme_start_p (datafeed()) in
    Printf.eprintf "At line %d and column %d: syntax error === %s. State is %s.\n%!"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Lexing.lexeme (datafeed())) (
      match !state with
      | Lexer.BaseLevel -> "BaseLevel"
      | Lexer.CommentLevel n -> "CommentLevel " ^ (string_of_int n) 
      | Lexer.HTMLLevel n -> "HTMLLevel " ^ (string_of_int n)
      | Lexer.InputAttrLevel (n, b) -> "InputAttrLevel " ^ (string_of_int n) ^ " " ^ (string_of_bool b)
      | Lexer.AttrIDLevel (n, b) -> "AttrIDLevel " ^ (string_of_int n) ^ " " ^ (string_of_bool b)
     )

  | Pretty.IndentationFailure ->
      Printf.eprintf "Syntax tree error: Attempted to reach a negative indentation level\n"

  | Weed.FuncNoReturn (IdentifierConst id) ->
      Printf.eprintf "Weeding error: Function %s does not always return\n" id
  | Weed.SessNoExit (IdentifierConst id) ->
      Printf.eprintf "Weeding error: Session %s does not always exit\n" id
  | Weed.FuncNotFound (IdentifierConst id) ->
      Printf.eprintf "Syntax tree error: Session references a function %s which cannot be found\n" id
  | FuncTypeError (IdentifierConst id) ->
      Printf.eprintf "Function %s was called with improper arguments\n" id
  | Duplicate (IdentifierConst id) ->
      Printf.eprintf "The identifier %s was defined more than once.\n" id
  | UndefType (IdentifierConst id) ->
      Printf.eprintf "The tuple type %s was used but never defined.\n" id
  | MissingDeclaration (IdentifierConst id) ->
      Printf.eprintf "The identifier %s was used without being declared.\n" id
  | SymbolTableFailed ->
      Printf.eprintf "Invalid operation on symbol table\n"
  | TypeError (exp, s, t1, t2) ->
      Printf.eprintf "Type Error! In expression %s: the %s operator got %s but expected %s\n" (Semantic.expToString exp) s (Semantic.idtypeToString t1) (Semantic.idtypeToString t2)
  | StmTypeError (stm, s, t1, t2) ->
      Printf.eprintf "Statement Error! In expression %s: the %s operator got %s but expected %s\n" (Semantic.stmToString stm) s (Semantic.idtypeToString t1) (Semantic.idtypeToString t2)
  | Typecheck.InvalidReceiveType (IdentifierConst id) ->
      Printf.eprintf "Typecheck error! Receive %s can only obtain strings or integers\n" id
  | Typecheck.InvalidPlugType (IdentifierConst id) ->
      Printf.eprintf "Typecheck error! Plug %s should only be used with strings, ints and bools\n" id
end

let () = main




