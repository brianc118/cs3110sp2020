type t = int list
(** Abstraction function: the list [x1; ...; xn] represents the set
    {x1, ..., xn}.  [] represents the empty set.
    Representation invariant: the list is sorted and contains no
    duplicates. *)

let rec rep_ok = function
  | [] -> []
  | [ h ] -> [ h ]
  | h :: h2 :: t ->
      if h >= h2 then raise (Failure "rep_not_ok") else h :: rep_ok (h2 :: t)

let empty = []

let size s = List.length s

let insert x s = x :: s |> List.sort_uniq Stdlib.compare |> rep_ok

let member x s = List.find_opt (( = ) x) s <> None

let remove x s =
   List.fold_left (fun acc h -> if h = x then acc else h :: acc) [] s |> rep_ok

let choose = function [] -> None | h :: _ -> Some h

let to_list s = s
