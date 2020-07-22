let double x = 2 * x

let square x = x * x

let twice f x = f (f x)

let quad = twice double

let fourth = twice square

let ( $ ) f x = f x

let _ = square $ 2 + 2

let _ = square 2 + 2

let ( @@ ) f g x = x |> g |> f

let _ = (String.length @@ string_of_int) 1

let _ = (String.length @@ string_of_int) 12

(* Exercise: repeat [✭✭] *)
let rec repeat f n x = if n = 0 then x else f (repeat f (n - 1) x)

let _ = repeat double 0 1

let _ = repeat double 10 1

(* Exercise: product [✭] *)
let product_left = List.fold_left ( *. ) 1.0

let product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0

(* Exercise: clip [✭✭] *)
let clip n = if n < 0 then 0 else if n > 10 then 10 else n

let cliplist lst = List.map clip lst

let rec cliplist_rec = function [] -> [] | h :: t -> clip h :: cliplist_rec t

(* Exercise: sum_cube_odd [✭✭] *)
let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

let sum_cube_odd n =
  List.fold_left ( + ) 0
    (List.map
       (fun x -> x * x * x)
       (List.filter (fun x -> x mod 2 = 1) (0 -- n)))

let _ = sum_cube_odd 10

(* Exercise: sum_cube_odd pipeline [✭✭] *)
let sum_cube_odd_pipeline n =
  List.(
    filter (fun x -> x mod 2 = 1) (0 -- n)
    |> map (fun x -> x * x * x)
    |> fold_left ( + ) 0)

(* Exercise: exists [✭✭] *)
let rec exists_rec p = function
  | [] -> false
  | h :: t -> if p h then true else exists_rec p t

let exists_fold p = List.fold_left (fun acc elt -> acc || p elt) false

let exists_lib = List.exists

let _ = assert (exists_rec (fun x -> x = 1) [ 0; 1; 3 ])

let _ = assert (exists_fold (fun x -> x = 1) [ 0; 1; 3 ])

let _ = assert (exists_lib (fun x -> x = 1) [ 0; 1; 3 ])

(* Exercise: budget [✭✭✭] *)
let budget_left budget = List.fold_left ( - ) budget

let budget_right budget expenses = budget - List.fold_right ( + ) expenses 0

let rec budget_rec budget expenses =
  match expenses with [] -> budget | h :: t -> budget_rec budget t - h

let _ = assert (budget_left 100 [ 1; 2; 3; 4; 5; 6 ] = 79)

let _ = assert (budget_right 100 [ 1; 2; 3; 4; 5; 6 ] = 79)

let _ = assert (budget_rec 100 [ 1; 2; 3; 4; 5; 6 ] = 79)

(* Exercise: library uncurried [✭✭] *)
(* Exercise: uncurry [✭✭] *)
let uncurry f (a, b) = f a b

let uncurried_nth a = uncurry List.nth a

let uncurried_append a = uncurry List.append a

let uncurried_compare = uncurry Char.compare

let uncurried_max a = uncurry max a

(* Exercise: terse product [✭✭, advanced] *)
(* already terse *)

(* Exercise: map composition [✭✭✭] *)

let f x = x + 1

let g x = x + 2

let _ = List.map f (List.map g [ 1; 2; 3 ])

let _ = List.map (fun x -> x |> f |> g) [ 1; 2; 3 ]

(* Exercise: more list fun [✭✭✭] *)

let greater_than_3 = List.filter (fun x -> x > 3)

let add_one = List.map (fun x -> x +. 1.0)

let join_strings strs sep =
  List.fold_left
    (fun acc elt -> if acc = "" then elt else acc ^ sep ^ elt)
    "" strs

let _ = assert (greater_than_3 [ 5; 1; 3; 4; 2 ] = [ 5; 4 ])

let _ = assert (add_one [ 5.0; 1.0; 3.0; 3.1; 2.0 ] = [ 6.; 2.; 4.; 4.1; 3. ])

let _ = assert (join_strings [ "hi"; "bye" ] "," = "hi,bye")

(* Exercise: tree map [✭✭✭] *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t =
  Node
    ( 4,
      Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
      Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )

let rec tree_map f = function
  | Leaf -> Leaf
  | Node (value, left, right) -> Node (f value, left, right)

let add1 = tree_map (fun x -> x + 1)

let _ =
  assert (
    add1 t
    = Node
        ( 5,
          Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
          Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) ) )

(* Exercise: association list keys [✭✭✭] *)
(* let keys key_vals = key_vals |>  *)
let d = [ (1, 5); (3, 5); (4, 5); (1, 5); (3, 5) ]

let keys key_vals = key_vals |> List.map fst |> List.sort_uniq compare

(* Exercise: valid matrix [✭✭✭] *)
let is_valid_matrix = function
  | [] -> false
  | h :: t ->
      List.length h > 0
      && List.for_all (fun x -> List.length h = List.length x) t

(* Exercise: row vector add [✭✭✭] *)
let add_row_vectors = List.map2 (fun a b -> a + b)

(* Exercise: matrix add [✭✭✭, advanced] *)
let add_matrices = List.map2 add_row_vectors

(* Exercise: matrix multiply [✭✭✭✭, advanced] *)
let transpose mat =
  List.fold_right
    (fun row tmat -> List.map2 List.cons row tmat)
    mat
    (List.init (List.nth mat 0 |> List.length) (fun _ -> []))

let dot a b = List.fold_right ( + ) (List.map2 ( * ) a b) 0

let multiply_matrices a b =
  let bt = transpose b in
  List.map (fun row -> List.map (fun col -> dot row col) bt) a

let multiply_matrices a b =
  transpose b
  |> List.map (fun row -> List.map (fun col -> dot row col) a)
  |> transpose
