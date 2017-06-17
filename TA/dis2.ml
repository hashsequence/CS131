let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
  match l with
    [] -> acc
  | x :: xs -> f x (fold_right f xs acc)

let rec fold_left (f : 'a -> 'b ->'a) (acc : 'a) (l : 'b list): 'a =
  match l with
    [] -> acc
  | h :: t -> fold_left f (f acc h) t

let (revR : 'a list -> 'a list) = 
fun l ->
List.fold_right (fun h t -> t@[h]) l []

let (mapR: ('a -> 'b) -> 'a list -> 'b list) =
fun f l ->
List.fold_right (fun t rest -> (f t)::rest) l []

let lengthL (l : 'a list) : int  = fold_left (fun acc h -> acc + 1) 0 l

let revL l = fold_left (fun acc e -> e::acc) [] l

let mapL f l = fold_left (fun acc e -> acc@[f e]) [] l

let (insertion_sort: 'a list -> 'a list) =
fun l -> fold_left
(fun acc x ->
let rec insert =
fun acc1 x1 ->
match acc1 with
[] -> x1::[]
| h::t -> if x1 < h then x1::acc1 else h::(insert t x1) in 
insert acc x)
[] l


       
