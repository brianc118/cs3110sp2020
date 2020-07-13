(* Exercise: list expressions [✭] *)

let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Exercise: product [✭✭] *)
let rec product lst = match lst with [] -> 1 | h :: t -> h * product t

let _ = assert (product [ 1; 2; 3 ] = 6)

(* Exercise: concat [✭✭, optional] *)
let rec concat lst = match lst with [] -> "" | h :: t -> h ^ concat t

let _ = assert (concat [ "ab"; "cd"; "ef" ] = "abcdef")

(* Exercise: product test [✭✭, optional] *)
(* Done. See exercises_test.ml *)

(* Exercise: patterns [✭✭✭] *)
let first_element_bigred lst =
  match lst with [] -> false | h :: _ -> h = "bigred"

let _ = assert (first_element_bigred [ "bigred"; "hi" ])

let _ = assert (not (first_element_bigred [ "hi"; "world" ]))

let two_or_four_elements lst =
  match lst with [ _; _ ] -> true | [ _; _; _; _ ] -> true | _ -> false

let _ = assert (two_or_four_elements [ 1; 2 ])

let _ = assert (two_or_four_elements [ 1; 2; 3; 4 ])

let _ = assert (not (two_or_four_elements [ 1; 2; 3 ]))

(* Exercise: library [✭✭✭] *)
let fifth_in_list lst = if List.length lst < 5 then 0 else List.nth lst 4

let _ = assert (fifth_in_list [ 1; 2; 3; 4; 5 ] = 5)

let _ = assert (fifth_in_list [ 1; 2; 3; 4 ] = 0)

let sorted_descending lst = List.rev (List.sort Stdlib.compare lst)

let _ = assert (sorted_descending [ 3; 2; 4; 1 ] = [ 4; 3; 2; 1 ])

(* Exercise: library test [✭✭✭, optional] *)
(* See exercises_test.ml *)

(* Exercise: take drop [✭✭✭] *)
let rec take n lst =
  if n > 0 then match lst with [] -> [] | h :: t -> h :: take (n - 1) t
  else []

let _ = assert (take 3 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3 ])

let _ = assert (take 3 [ 1; 2 ] = [ 1; 2 ])

let rec drop n lst =
  if n <= 0 then lst else match lst with [] -> [] | _ :: t -> drop (n - 1) t

let _ = assert (drop 3 [ 1; 2; 3; 4; 5 ] = [ 4; 5 ])

let _ = assert (drop 3 [ 4; 5 ] = [])

(* Exercise: take drop tail [✭✭✭✭, recommended] *)

(* returns:  [from i j l] is the list containing the integers from
 *   [i] to [j], inclusive, followed by the list [l].
 * example:  [from 1 3 [0] = [1;2;3;0]] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(* returns:  [i -- j] is the list containing the integers from
 *   [i] to [j], inclusive.
 *)
let ( -- ) i j = from i j []

let longlist = 0 -- 1_000_000

(* drop is already tail recursive but take isn't *)

let rec take_acc n acc lst =
  if n > 0 then
    match lst with [] -> acc | h :: t -> take_acc (n - 1) (h :: acc) t
  else acc

let rec take_tr n lst = List.rev (take_acc n [] lst)

let _ = take_tr 1_000_000 longlist

let _ = drop 1_000_000 longlist

(* Exercise: unimodal [✭✭✭] *)
let rec unimodal_helper lst decreasing =
  match lst with
  | a :: b :: t ->
      if a > b then unimodal_helper (b :: t) true
      else if decreasing && a < b then false
      else unimodal_helper (b :: t) decreasing
  | _ -> true

let unimodal lst = unimodal_helper lst false

let _ = assert (unimodal [ 1; 2; 3; 2; 1 ])

let _ = assert (unimodal [ 1; 2; 3; 3; 2 ])

let _ = assert (unimodal [ 1; 1; 2; 1 ])

let _ = assert (unimodal [ 3; 2; 1 ])

let _ = assert (unimodal [ 1; 2; 3 ])

let _ = assert (not (unimodal [ 2; 1; 2 ]))

(* Exercise: powerset [✭✭✭] *)
let rec add_to_elements x lst =
  match lst with [] -> [] | h :: t -> (x :: h) :: add_to_elements x t

let rec powerset lst =
  match lst with
  | [] -> [ [] ]
  | x :: s -> add_to_elements x (powerset s) @ powerset s

let _ = powerset [ 3 ]

let _ = powerset [ 2; 3 ]

let _ = powerset [ 1; 2; 3 ]

(* Exercise: print int list rec [✭✭] *)
let rec print_int_list lst =
  match lst with
  | [] -> ()
  | x :: t ->
      print_int x;
      print_endline "";
      print_int_list t

let _ = print_int_list [ 1; 2; 3 ]

(* Exercise: print int list iter [✭✭] *)
let print_int_list' lst =
  List.iter
    (fun x ->
      print_int x;
      print_endline "")
    lst

let _ = print_int_list' [ 4; 5; 6 ]

(* Exercise: student [✭✭] *)
type student = { first_name : string; last_name : string; gpa : float }

let student_name stu = (stu.first_name, stu.last_name)

let create_student ~first_name:a ~last_name:b ~gpa =
  { first_name = a; last_name = b; gpa }

let _ = student_name (create_student "John" "Smith" 3.0)

(* Exercise: pokerecord [✭✭] *)
type poketype = Normal | Fire | Water

type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }

let squirtle = { name = "squirtle"; hp = 44; ptype = Water }

(* Exercise: safe hd and tl [✭✭] *)
let safe_hd lst = match lst with [] -> None | h :: _ -> Some h

let rec safe_tl lst =
  match lst with [] -> None | [ t ] -> Some t | _ :: t -> safe_tl t

let _ = safe_hd []

let _ = safe_hd [ 1; 2; 3 ]

let _ = safe_tl []

let _ = safe_tl [ 1; 2; 3 ]

(* Exercise: pokefun [✭✭✭] *)

let rec max_hp_helper lst cur_poke =
  match lst with
  | [] -> Some cur_poke
  | h :: t ->
      if h.hp > cur_poke.hp then max_hp_helper t h else max_hp_helper t cur_poke

let rec max_hp lst = match lst with [] -> None | h :: t -> max_hp_helper t h

let _ = max_hp [ charizard; squirtle ]

(* Exercise: date before [✭✭] *)

let is_before d1 d2 =
  match d1 with
  | y1, m1, d1 -> (
      match d2 with
      | y2, m2, d2 ->
          y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y1 && m1 = m2 && d1 < d2) )

let _ = assert (not (is_before (1, 2, 3) (1, 2, 3)))

let _ = assert (not (is_before (1, 2, 3) (1, 1, 1)))

(* Exercise: earliest date [✭✭✭] *)

let rec earliest_helper lst cur_min =
  match lst with
  | [] -> Some cur_min
  | h :: t ->
      if is_before h cur_min then earliest_helper t h
      else earliest_helper t cur_min

let earliest lst = match lst with [] -> None | h :: t -> earliest_helper t h

let _ =
  assert (
    earliest [ (3, 2, 1); (4, 3, 2); (4, 1, 2); (1, 2, 1); (1, 2, 2) ]
    = Some (1, 2, 1) )

(* Exercise: assoc list [✭] *)

(* insert a binding from key k to value v in association list d *)
let insert k v d = (k, v) :: d

(* find the value v to which key k is bound, if any, in the assocation list *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let d = insert 1 "one" (insert 2 "two" (insert 3 "three" []))

let _ = lookup 2 d

let _ = lookup 4 d

(* Exercise: cards [✭✭] *)
type suit = Clubs | Diamonds | Hearts | Spades

type rank =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

type card = { suit : suit; rank : rank }

let ace_of_clubs = { suit = Clubs; rank = Ace }

let queen_of_hearts = { suit = Hearts; rank = Queen }

let two_of_diamonds = { suit = Diamonds; rank = Two }

let seven_of_spades = { suit = Spades; rank = Seven }

(* Exercise: matching [✭] *)

(* For each pattern in the list below, give a value of type int option list that does not match the pattern and is not the empty list, or explain why that's impossible.
 *
 * (Some x)::tl
 *     [ None ]
 * [Some 3110; None]
 *     [ Some 0 ]
 * [Some x; _]
 *     [ None; Some 1 ]
 * h1::h2::tl
 *     [ Some 1; Some 2 ]
 * h :: tl 
 *     [ None ]
 *
 *)

(* Exercise: quadrant [✭✭] *)
type quad = I | II | III | IV

type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x > 0 then Pos else if x < 0 then Neg else Zero

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _, _ -> None

let _ = assert (quadrant (1, 1) = Some I)

let _ = assert (quadrant (1, 0) = None)

(* Exercise: quadrant when [✭✭] *)
let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | x, y -> None

let _ = assert (quadrant_when (1, 1) = Some I)

let _ = assert (quadrant_when (1, 0) = None)

(* Exercise: depth [✭✭] *)
type 'a tree = Leaf | Node of 'a node

and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

let ta =
  Node
    {
      value = 2;
      left = Node { value = 1; left = Leaf; right = Leaf };
      right = Node { value = 3; left = Leaf; right = Leaf };
    }

let tb =
  Node
    {
      value = 3;
      left = Node { value = 4; left = Leaf; right = Leaf };
      right = Node { value = 7; left = Leaf; right = Leaf };
    }

let tc = Node { value = 4; left = ta; right = Leaf }

let td = Node { value = 4; left = tb; right = Leaf }

let rec depth = function
  | Leaf -> 0
  | Node { left; right } -> 1 + max (depth left) (depth right)

let _ = assert (depth ta = 2)

(* Exercise: shape [✭✭✭] *)
let rec same_shape_helper = function
  | Leaf, Leaf -> true
  | Leaf, _ -> false
  | _, Leaf -> false
  | Node { left = l1; right = r1 }, Node { left = l2; right = r2 } ->
      same_shape_helper (l1, l2) && same_shape_helper (r1, r2)

let same_shape a b = same_shape_helper (a, b)

let _ = assert (same_shape ta tb)

let _ = assert (not (same_shape ta tc))

(* Exercise: list max exn [✭✭] *)
let rec list_max_helper cur_max = function
  | [] -> cur_max
  | h :: t -> list_max_helper (max cur_max h) t

let list_max = function
  | [] -> raise (Failure "list_max")
  | h :: t -> list_max_helper h t

let _ = list_max [ 4; 3; 5; 2 ]

(* Exercise: list max exn string [✭✭] *)
let list_max_string = function
  | [] -> "empty"
  | h :: t -> string_of_int (list_max_helper h t)

(* Exercise: is_bst [✭✭✭✭] *)
type bst_result = Empty | Node of int * int | Invalid

let rec is_bst_helper = function
  | Leaf -> Empty
  | Node { value; left; right } -> (
      match (is_bst_helper left, is_bst_helper right) with
      | Empty, Empty -> Node (value, value)
      | Empty, Node (l, u) -> if value < l then Node (value, u) else Invalid
      | Node (l, u), Empty -> if value > u then Node (l, value) else Invalid
      | Node (l1, u1), Node (l2, u2) ->
          if value > u1 && value < l2 then Node (l1, u2) else Invalid
      | _, _ -> Invalid )

let is_bst root = match is_bst_helper root with Invalid -> false | _ -> true

let _ = assert (is_bst ta)

let _ = assert (not (is_bst tb))

let _ = assert (is_bst tc)

let _ = assert (not (is_bst td))

(* Exercise: quadrant poly [✭✭] *)
let sign x = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

let quadrant (x, y) =
  match (sign x, sign y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _, _ -> None

let _ = assert (quadrant (1, 1) = Some `I)

let _ = assert (quadrant (1, 0) = None)
