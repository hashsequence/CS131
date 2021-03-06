
(* You don't need to understand or modify anything in this file *)

(* Produce the AST associated with the given string. *)
let toAST (s: string) : modecl =
  main token (Lexing.from_string (s^";;"))


(* PRETTY PRINTING *)

let rec print_tuple (vs: movalue list) :string =
  match vs with
    [] -> ""
  | [v] -> print_val v
  | v::vs -> (print_val v) ^ ", " ^ (print_tuple vs)
and print_val (v:movalue) :string =
  match v with
      IntVal(i) -> string_of_int i
    | BoolVal(b) -> string_of_bool b
    | FunctionVal(_) -> "<fun>"
    | TupleVal vs -> "(" ^ (print_tuple vs) ^ ")"
    | DataVal(c, vopt) ->
       match vopt with
         None -> c
       | Some v -> c ^ " " ^ (print_val v)

let print_result ((nopt,v):moresult) :string =
  let vstr = print_val v in
  match nopt with
      None -> vstr
    | Some x -> "val " ^ x ^ " = " ^ vstr

      
(* ENTRY POINT *)	  

let readEvalPrint(env:moenv) :moenv =
  let _ = print_string "mocaml# "; flush stdout in
  let lexbuf = Lexing.from_channel stdin in
  let decl = main token lexbuf in
  let result = evalDecl decl env in
    print_string (print_result result); print_newline(); flush stdout;
    match result with
	(None,_) -> env
      | (Some x,v) -> Env.add_binding x v env

let rec repl(env:moenv) =
  try
    repl(readEvalPrint(env))
  with
      Eof ->
	exit 0
    | Parsing.Parse_error ->
	print_string "parse error";
	print_newline(); flush stdout; repl(env)
    | DynamicTypeError s ->
	print_string "error: "; print_string s;
	print_newline(); flush stdout; repl(env)
    | MatchFailure ->
	print_string "match failure";
	print_newline(); flush stdout; repl(env)
    | ImplementMe(s) ->
	print_string s;
	print_newline(); flush stdout; repl(env)

	  
let mocaml() =
  print_newline();
  print_string "**********Welcome to MOCaml!**********";
  print_newline(); flush stdout;
  repl(Env.empty_env())

