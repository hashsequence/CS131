0(* Name: Avery Wong

   UID: 904582269

   Others With Whom I Discussed Things: Ran Gong

   Other Resources I Consulted:
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

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
    (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
  | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
  | (WildcardPat,_) -> Env.empty_env()
  (*when I match a variable I am adding a binding to the environment*)
  | (VarPat(s), variable) -> Env.add_binding s variable (Env.empty_env())
  | (TuplePat(mopatlist), TupleVal(movallist)) -> (match(mopatlist,movallist) with
                                                    ([],[]) -> Env.empty_env()
                                                  | (h1::t1,h2::t2) -> Env.combine_envs (patMatch h1 h2) (patMatch (TuplePat(t1)) (TupleVal(t2)))
                                                  | _ -> raise MatchFailure)
  | (DataPat(s1,pato1),DataVal(s2,valo1)) when s1=s2 -> (match (pato1,valo1) with
                                                          (Some(pato),Some(valo)) -> patMatch pato valo
                                                        | (None,None) -> Env.empty_env()
                                                        | _ -> raise MatchFailure)
(*  | _ -> raise (ImplementMe "pattern matching not implemented")*)
  | _ -> raise MatchFailure
       
    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
   |BoolConst(b) -> BoolVal(b)
   |Var(s) ->(try (Env.lookup s env) with Env.NotBound -> raise (DynamicTypeError "Var: Error") )
   |BinOp(moexpr1,operator,moexpr2) -> (match ((evalExpr moexpr1 env) ,operator, (evalExpr moexpr2 env)) with
                                         (IntVal(a),Plus,IntVal(b)) -> IntVal(a + b)
                                       | (IntVal(a),Minus,IntVal(b)) -> IntVal(a - b)
                                       | (IntVal(a),Times,IntVal(b)) -> IntVal(a * b)
                                       | (IntVal(a),Eq, IntVal(b)) -> BoolVal ( a = b)
                                       | (IntVal(a),Gt, IntVal(b)) -> BoolVal ( a > b)
                                       | _ -> raise (DynamicTypeError "Binop: Error"))
   | Negate(e1) -> (match evalExpr e1 env with IntVal(e2) -> IntVal(-e2) | _ -> raise (DynamicTypeError "Negate: Error"))
   | If (guard, thn, els) -> (match evalExpr guard env with
                                BoolVal true -> evalExpr thn env
                              | BoolVal false -> evalExpr els env
                              | _ -> raise (DynamicTypeError "If: Error") )
   | Function(p1,e1) -> FunctionVal(None,p1,e1,env)
   (*for functioncalls evalExpr e1 should return the movalue for the function name and evalExpr e3 should return the movalue for the argument
if the function call is recursive then there will be Some(name) that means I have to evaluate the expression in the function in the context of the
combined environment of the argument and the environment of the function itself from the the caller, so the scoping is correct when evaluating the function call
there are two cases nonrecursive calls and recursive calls:

if the function call is recursive then there is a Some name so for the function's environment I have to add a binding with the function name and the functionVal, which is how I interpreted the specs of hw3,

for FunctionVal(a,b,c,d) I interpret as 

a: is the function_name which is either none if the functionCall was not recrusive and Some name otherwise
b: is the mopath which correspond to the argument and I can look up the environment for the argument using the patMatch
c: is the body of the expression
d: is the environment for the function

CASE 1: recursive function

step 1: use patmatch to get the bindings for the argument by evaluating the argument first then retrieve the environment
step 2: add a binding of te function name with the FunctionValuse to the environment so the mapping from the stored name to the function value should first be added to the environment, ensuring that references to the function's name in its own body will be handled properly., this gives you a second environment
step 3: combine all the environments, make sure that the environment for the argument overshadows the one in step 2 because consider the case

let f=2;;
let rec f f = f + 7;;
f 3;;

the only way that the function know that f is an argument and not an error is if the binding on f is with 2

CASE 2: non recursive
not a recursive function so None, this case is easier since the environment for the function is just b_env, so
I just have to add the binding for the paramater which I can do using patmatch on the parameter then combine the environments overshadowing the environment in the function body

    *)
    
   | FunctionCall(e1,e2) -> (match ((evalExpr e1 env),(evalExpr e2 env)) with 
                               FunctionVal(Some(a_funcname),a_pat,a_expr,a_env),a_param ->
                                (evalExpr a_expr (Env.combine_envs (Env.add_binding a_funcname (FunctionVal(Some(a_funcname),a_pat,a_expr,a_env)) a_env) (patMatch a_pat a_param)))
                           | FunctionVal(None,b_pat,b_expr,b_env),b_param ->
                                (evalExpr b_expr (Env.combine_envs b_env (patMatch b_pat b_param)))
                           | (_,_) -> raise (DynamicTypeError "FunctionCall: Error"))
(*                          
   | Match(e1,e2_list) -> (match ((evalExpr e1 env),e2_list) with
                            (e1_expr,(a_pat,a_expr)::rest) ->
                            ( try evalExpr a_expr (Env.combine_envs env (patMatch a_pat e1_expr))
                              with MatchFailure -> (match rest with
                                                    [] -> raise MatchFailure
                                                    | _ ->
                                                      (evalExpr (Match(e1,rest)) env)))                             
                           | _ -> raise (DynamicTypeError "match error"))
 *) 
  | Match(e1,e2_list) -> (match ((evalExpr e1 env),e2_list) with
                            (e1_expr,(a_pat,a_expr)::rest) ->
                            (
                            try
                            (
                              let step1 = (try (patMatch a_pat e1_expr) with MatchFailure -> (raise (DynamicTypeError "Intermediate")))
                              in (evalExpr a_expr (Env.combine_envs env step1))
                            )
                            with
                              (DynamicTypeError "Intermediate") -> match rest with
                                                [] -> raise MatchFailure
                                              | _ -> evalExpr (Match(e1,rest)) env 
                            )
                          | _ -> raise (DynamicTypeError "Match: error"))                                              
   | Tuple(expr_list) -> TupleVal((List.map (function ele -> evalExpr ele env) expr_list))
   | Data(a,b) -> (match (a,b) with
                     ("Cons", Some (Tuple [IntConst(_); IntConst(_)])) -> raise (DynamicTypeError "Data: Error")
                   | ("Cons", Some (Tuple [BoolConst(_); BoolConst(_)])) -> raise (DynamicTypeError "Data: Error")
                   | (s1,None) -> DataVal(s1, None) 
                   | (s1, Some(s2)) -> DataVal(s1,Some(evalExpr s2 env)))
(*| _ -> raise (ImplementMe "expression evaluation not implemented")*)


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
    Expr(e) -> (None, evalExpr e env)
  | Let(a,b) -> (Some(a), evalExpr b env)
  | LetRec(a,b) -> (match b with Function(e1,e2) -> (Some(a), FunctionVal(Some(a),e1,e2,env)) | _ -> raise (DynamicTypeError "evalDecl Error"))

                                                     (*| _ -> raise (ImplementMe "let and let rec not implemented")*)
                              
       
    
