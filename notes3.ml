let rec nth (n:int) (l : 'a list) : 'a option =
  match n with
    0 ->
    (match l with
       [] -> None
     | h::t -> Some h)
  | _ when n < 0 -> None
  | _ ->
     (match l with
        [] -> None
      | h::t -> nth (n-1) t
     )

let rec nth2 n l =
  match l with
    []-> None
   |h::t ->
     if n=0 then Some h
     else if n < 0 then None
     else nth2 (n-1) t

let rec nth3 n l =
  match (n,l) with
    (_,[]) -> None
  | (_,_) when n < 0 -> None
  | (0, h::t) -> Some h
  | (_,h::t) -> nth3 (n-1) t

type intlist = Empty | Node of int * intlist

                          
(*[1;2;3];;
 Node(1, Node(2, Node(3,Empty)))*)

let rec toIntList l =
  match l with
    [] -> Empty
  | h::t -> Node (h , toIntList t)

let rec fromIntList l =
  match l with
    Empty -> []
  | Node(h,t) -> h :: (fromIntList t)

type 'a mylist = Empty | Node of 'a * 'a mylist
                               
let rec mymap f l =
  match l with
    Empty -> Empty
  | Node(h,t) -> Node(f h, mymap f t)

type 'a mytree = Leaf | InternalNode of 'a * 'a mytree * 'a mytree

let rec size t =
  match t with
    Leaf -> 0
  | InternalNode(_, l, r) ->
     1 + (size l) + (size r) 

let rec preorder ( t: 'a mytree) : 'a list =
  match t with
    Leaf -> []
  | InternalNode( d, l, r) ->
     d :: (preorder l) @ (preorder r)

let rec insert x t =
  match t with
    Leaf -> InternalNode( x, Leaf, Leaf)
  | InternalNode(h,l,r)->
     if x < h then
       InternalNode(h, insert x l, r)
     else
       InternalNode(h, l, insert x r)
