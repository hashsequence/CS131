type peano = zero | succ of zero

let rec (toPeano : int -> peano) =
  fun i -> match i with
           | 0 -> zero
           | n -> succ (toPeano (n-1))

let rec (add : peano -> peano -> peano) =
  fun p1 p2 ->
  match p1 with
    zero -> p2
    | succ(q) -> add(q ( succ p2))

let rec (mul : peano -> peano -> peano) =
  fun p1 p2 ->
  match p1 with
    zero -> zero
    succ(q) -> add p2 (mul q p2)
