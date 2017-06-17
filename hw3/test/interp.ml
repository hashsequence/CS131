(* Name: David Peatman

   UID: 004027369

   Others With Whom I Discussed Things:

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
    | (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | BoolPat(b1),BoolVal(b2) when b1=b2 -> Env.empty_env()
    | NilPat,ListVal(NilVal) -> Env.empty_env()
    | ConsPat(p1,p2),ListVal(ConsVal(first,rest)) ->
            (Env.combine_envs (patMatch p1 first) (patMatch p2 (ListVal rest)))
    | VarPat(s),v -> (Env.add_binding s v (Env.empty_env()))
    | WildcardPat,_ -> Env.empty_env()
    | _ -> raise MatchFailure

    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)

exception MatchContinue

let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
      IntConst(i) -> IntVal(i)
    | BoolConst(b) -> BoolVal(b)
    | Nil -> ListVal(NilVal)
    | Var(s) -> (try Env.lookup s env with
                Env.NotBound -> raise DynamicTypeError)
    | BinOp (e1,o,e2) -> (match o with
                           | Plus -> (match (evalExpr e1 env,evalExpr e2 env) with
                                       | IntVal(i1),IntVal(i2) -> IntVal(i1 + i2)
                                       | _ -> raise DynamicTypeError)
                           | Minus -> (match (evalExpr e1 env,evalExpr e2 env) with
                                       | IntVal(i1),IntVal(i2) -> IntVal(i1 - i2)
                                       | _ -> raise DynamicTypeError)
                           | Times -> (match (evalExpr e1 env,evalExpr e2 env) with
                                       | IntVal(i1),IntVal(i2) -> IntVal(i1 * i2)
                                       | _ -> raise DynamicTypeError)
                           | Eq -> (match (evalExpr e1 env,evalExpr e2 env) with
                                       | IntVal(i1),IntVal(i2) -> BoolVal(i1 = i2)
                                       | BoolVal(b1),BoolVal(b2) -> BoolVal(b1 = b2)
                                       | _ -> raise DynamicTypeError)
                           | Gt -> (match (evalExpr e1 env,evalExpr e2 env) with
                                       | IntVal(i1),IntVal(i2) -> BoolVal(i1 > i2)
                                       | _ -> raise DynamicTypeError)
                           | Cons -> (match (evalExpr e1 env,evalExpr e2 env) with
                                       | v,ListVal(l) -> ListVal(ConsVal(v,l))
                                       | _ -> raise DynamicTypeError))
    | Negate(e) -> (match (evalExpr e env) with
                     | IntVal(i) -> IntVal(-i)
                     | _ -> raise DynamicTypeError)
    | If(e1,e2,e3) -> (match (evalExpr e1 env) with
                        | BoolVal(b) -> if b
                                        then (evalExpr e2 env)
                                        else (evalExpr e3 env)
                        | _ -> raise DynamicTypeError)
    | Function(p,e) -> FunctionVal(None, p, e, env)

    | FunctionCall(e1,e2) -> (match (evalExpr e1 env),(evalExpr e2 env) with
                               | FunctionVal(s,p,e,fenv),arg -> 
                                    (match s with
                                      | None -> let fenv = (Env.combine_envs fenv (patMatch p arg))
                                                in (evalExpr e fenv)
                                      | Some(name) -> let fenv = (Env.combine_envs (Env.add_binding name (FunctionVal(Some name, p, e, fenv)) fenv) (patMatch p arg))
                                                      in (evalExpr e fenv))
                               | _ -> raise DynamicTypeError)
     (*
     | FunctionCall(e1,e2) -> (match ((evalExpr e1 env),(evalExpr e2 env)) with
                             FunctionVal(Some(a_funcname),a_pat,a_expr,a_env),a_param ->
                                (evalExpr a_expr (Env.add_binding a_funcname (FunctionVal(Some(a_funcname),a_pat,a_expr,a_env)) (Env.combine_envs a_env (patMatch a_pat a_param))))
                           | FunctionVal(None,b_pat,b_expr,b_env),b_param ->
                                (evalExpr b_expr (Env.combine_envs b_env (patMatch b_pat b_param)))
                           | (_,_) -> raise DynamicTypeError)
      *)    
    | Match(e,x::xs) -> (match x with
                          | pat,exp -> 
                            (try
                                (let env2 = (try (Env.combine_envs env (patMatch pat (evalExpr e env)))
                                             with MatchFailure -> if xs = []
                                                                  then raise MatchFailure
                                                                  else raise MatchContinue)
                                in (evalExpr exp env2))
                            with MatchFailure -> raise MatchFailure
                               | MatchContinue -> (evalExpr (Match(e,xs)) env))
                          | _ -> raise DynamicTypeError)
    | _ -> raise (ImplementMe "expression evaluation not implemented")

    
(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      Expr(e) -> (None, evalExpr e env)
    | Let (s, e) -> let env = (Env.add_binding s (evalExpr e env) env)
                    in (Some s, evalExpr (Var s)  env)
    | LetRec (s,p,e) -> let env = (Env.add_binding s (FunctionVal(Some s, p, e, env)) env)
                        in (Some s, FunctionVal(Some s, p, e, env))

