(* open Modules *)

(* Exercise: complex synonym [✭] *)

module type ComplexSig = sig
  type t = float * float

  val zero : t

  val add : t -> t -> t
end

(* Exercise: complex encapsulation [✭✭] *)

module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)

  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(* Exercise: big list queue [✭✭] *)
(* Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty

(* let _ = fill_listqueue 10000 *)

(* Exercise: big two-list queue [✭✭] *)
let fill_twolistqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (TwoListQueue.enqueue n q)
  in
  loop n TwoListQueue.empty

(* let _ = fill_twolistqueue 10000000 *)

(* Exercise: queue efficiency [✭✭✭] *)
(* ??? *)

(* Exercise: binary search tree dictionary [✭✭✭] *)
let d = BstDict.insert 4 "john" (BstDict.insert 3 "smith" BstDict.empty)

let _ = assert ("smith" = BstDict.lookup 3 d)

let _ = assert ("john" = BstDict.lookup 4 d)

(* Exercise: fraction [✭✭✭] *)
(* Exercise: fraction reduced [✭✭✭] *)

(* Exercise: make char map [✭] *)

(* Exercise: char ordered [✭] *)
(* Exercise: use char map [✭✭] *)
let d =
  CharMap.(
    empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra"
    |> add 'V' "Victor")

let _ = assert ("Echo" = CharMap.find 'E' d)

let _ =
  assert (
    CharMap.bindings d
    = [ ('A', "Alpha"); ('E', "Echo"); ('S', "Sierra"); ('V', "Victor") ] )

(* Exercise: bindings [✭✭] *)
(* Exercise: date order [✭✭] *)

let c : calendar =
  DateMap.(
    empty
    |> add { month = 12; day = 24 } "birthday"
    |> add { month = 1; day = 27 } "important")

(* Exercise: print calendar [✭✭] *)
let _ = print_calendar c

(* Exercise: is for [✭✭✭] *)
let d_is_for = is_for d

let _ =
  assert (
    CharMap.bindings d_is_for
    = [
        ('A', "A is_for Alpha");
        ('E', "E is_for Echo");
        ('S', "S is_for Sierra");
        ('V', "V is_for Victor");
      ] )

(* Exercise: first after [✭✭✭] *)
let _ = assert (first_after c { month = 2; day = 3 } = "birthday")

let _ = assert (first_after c { month = 1; day = 1 } = "important")

(* Exercise: sets [✭✭✭] *)
let _ =
  assert (CisSet.(equal (of_list [ "grr"; "argh" ]) (of_list [ "GRR"; "aRgh" ])))

(* Exercise: ToString [✭✭] *)
module type ToString = sig
  type t

  val to_string : t -> string
end

(* Exercise: Print [✭✭] *)
module Print (M : ToString) = struct
  let print v = M.to_string v
end

(* Exercise: Print Int [✭✭] *)
module Int = struct
  type t = int

  let to_string = string_of_int
end

module PrintInt = Print (Int)

(* Exercise: Print String [✭✭] *)
module MyString = struct
  type t = string

  let to_string s = s
end

module PrintString = Print (MyString)

(* Exercise: Print reuse [✭] *)

(* Exercise: Print String reuse revisited [✭✭] *)
module StringWithPrint = struct
  include String
  include PrintString
end

(* Exercise: implementation without interface [✭] *)

(* Exercise: implementation with interface [✭] *)

(* Exercise: implementation with abstracted interface [✭] *)