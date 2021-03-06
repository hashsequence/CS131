exception ImplementMe

(* Problem 1 *)
            
let rec (member : 'a -> 'a list -> bool) =
(*raise ImplementMe*)
fun x s ->
match s with
[] -> false
| head::rest ->
   if x = head then true
   else member x rest

let (add : 'a -> 'a list -> 'a list) =
  (*raise ImplementMe*)
fun x s ->
 if member x s then
  s
 else
  x::s

let rec (union : 'a list -> 'a list -> 'a list) =
 (* raise ImplementMe*)
fun s1 s2 ->
 match s1 with 
 [] -> s2
 | a::t ->
   if member a s2 then 
    (union t s2)
   else
    a::(union t s2)
           

let rec (fastUnion : 'a list -> 'a list -> 'a list) =
(*  raise ImplementMe *)
fun s1 s2 ->
match s1 with
 [] -> s2
 | h1::t1 ->
  match s2 with
   [] -> s1
   | h2::t2 ->
     if h1 > h2 then
      h2::(fastUnion s1 t2) 
     else if h1 < h2 then 
      h1::(fastUnion t1 s2)
     else 
      h1::(fastUnion t1 t2)
     
let (intersection : 'a list -> 'a list -> 'a list) =
(*raise ImplementMe*)
fun s1 s2 ->
 List.filter (fun a2 -> (member a2 s1)) s2
                
let rec (setify : 'a list -> 'a list) =
(* raise ImplementMe *)
fun s1 ->
 match s1 with
 [] -> [] 
 | head::rest ->
  add head (setify rest)

let rec (powerset : 'a list -> 'a list list) =
(*raise ImplementMe *)
fun s ->
 match s with
 [] -> [[]]
 | h::t ->
   let p = powerset t in
   (List.map (fun e -> h::e) p) @ p

(* Problem 2 *)        
        
let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
 (* raise ImplementMe*)
fun f s ->
 match s with 
 [] -> ([],[])
 | h::t -> 
  let (s1,s2) = partition f t in
  if f h then
   (h::s1,s2)
  else
   (s1,h::s2)

let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
  (*raise ImplementMe*)
fun p f v0 ->
 if p v0 then 
  whle p f (f v0)
 else
  v0
                                    
let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
  (*raise ImplementMe*)
fun n f ->
 match n with
 0 -> ( function x -> x ) 
 | 1 -> (function x -> f x)  
 | _ -> let composite = fun g h -> (fun x -> g(h(x))) in 
        composite f (pow (n-1) f) 
