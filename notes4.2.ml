let rec lengthbool (l : bool list) : int =
  match l with
    [] -> 0
  | _::t -> 1 + (lengthbool t)

let rec lengthint (l : int list) : int =
 match l with
    [] -> 0
  | _::t -> 1 + (lengthint t)

(*instead of doing the same functions I can us 'a list*)

let rec length (l : 'a list) : int =
   match l with
    [] -> 0
  | _::t -> 1 + (length t)

(*
'a is a *type variable*
'a is implicitly instantiated on each call to length
length (*int*) [1;2;3]
length(*bool*) [true;false]

in C we dont have type checking but we can kind of do this using void*
 *)

let rec (sumlist : 'a list -> int) =
  fun l -> match l with
    [] -> 0
  | h::t -> h + sumlist t

(* 
ocaml that does not support overloadinf so you cannot use + for floats 
you need to use +. 
partially due to type inferences
*)
          
let double x = x*2

(*val double : int -> int = <fun>*)
(*How to typecheck this call?
#double 45;;

-look up the type double : int -> int
-compute the type 45 : int
-check the actual and formal parameters types are equal
 *)
             
             
let pos x = x > 0
                  (* val pos : int -> bool = <fun> *)
          (*How to typecheck this call?
#pos 45;;
-look up the type pos : int -> bool
-compute the type 45 : int
-check that the actual and formal parameter types are equal
-the type of the whole thing is the result type of the function : bool

           *)

(*
How do we type check this call [true;false;true];;
-look up the type length : 'a list -> int
-compute the type : bool list
-check that we can find an instantiation of 'a with parameter types be equal 'a = bool
-the type of the whole thing is the result type of the function : bool
 *)

(*
How do we type check this call rotate (1, "hi", 3.14);
-look up the type rotate : ('a * 'b * 'c -> 'c * 'a * 'b)
-compute the type (1, "hi", 3.14) : int * string * float
-check that we can find an instantiation of 'a, 'b, and 'c 
 with types that makes the actual and formal parameter types be 
equal :
'a = int
'b = string
'c = float
-the type of the whole thing is the result type of the function : 
float * int * string
*)

(*
 key idea: parametric polymorphism
-one function
-can pass many different types of arguments
*)
          
(* contrast with static overloading:
multiple functions one name

list adding floats and ints are overloaded in C
but not in ocaml

# (+);;
- : int -> int -> int = <fun>
# (+.);;
- : float -> float -> float = <fun>

(>) and (=) is treated as polymorphically in ocaml 
*)
