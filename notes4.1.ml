let l1 = [(1,"hi");(3,"bi");(2,"yo")]

let rec lookup k l =
  match l with
    [] -> None
   |(key,v)::t ->
     if key=k then Some v else lookup k t

let lookupAndExclaim k t =
  let vopt = lookup k t in
  match vopt with
    None -> None
  | Some s -> Some (s^"!")

let rec lookupAll ks l =
  match ks with
    [] -> Some []
  | k::rest ->
     let vopt = lookup k l in match vopt with
                                None -> None
                               |Some v ->
                                 let vsopt = lookupAll rest l in
                                 match vsopt with
                                   None -> None
                                 |Some vs -> Some (v::vs)
(*exceptions:
-clear indication of an error
-clean seperation of error handling code from 
ordinary functionality
-allows callers to easily pass through the error
if they cant handle it
 *)
exception NotFound of string
exception AnotherOne 

let rec lookupE k l =
  match l with
    [] -> raise (NotFound "key not in list")
   |(key,v)::t ->
     if key=k then v else lookupE k t

(*implicitly propagates the exception*)    
let lookupAndExclaimE k l =
      (lookupE k l) ^ "!"

(*in this version, I want to return "error" if theres an error*)
let lookupAndExclaimTry k l =
  try
    (lookupE k l) ^ "!"
  with
    NotFound s-> "error:" ^ s
   |AnotherOne -> "another one"
   | _ -> "unknown bad thing"
                                       
let rec lookupAllE ks l =
   List.map (fun k -> lookupE k l) ks

let lookupAllTry ks l =
  try
    Some (List.map (fun k -> lookupE k l ) ks)
  with
    NotFound s -> None

let lookupAllTry2 ks l =
  List.map
    (fun k -> try Some (lookupE k l) with
                NotFound s -> None)
   ks
