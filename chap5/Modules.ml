module type Queue = sig
  (* An ['a queue] is a queue whose elements have type ['a]. *)
  type 'a queue

  (* The empty queue. *)
  val empty : 'a queue

  (* Whether a queue is empty. *)
  val is_empty : 'a queue -> bool

  (* [enqueue x q] is the queue [q] with [x] added to the end. *)
  val enqueue : 'a -> 'a queue -> 'a queue

  (* [peek q] is [Some x], where [x] is the element at the front of the queue,
     or [None] if the queue is empty. *)
  val peek : 'a queue -> 'a option

  (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
     of [q] except the front of [q], or [None] if [q] is empty. *)
  val dequeue : 'a queue -> 'a queue option
end

module ListQueue : Queue = struct
  (* Represent a queue as a list.  The list [x1; x2; ...; xn] represents
     the queue with [x1] at its front, followed by [x2], ..., followed
     by [xn]. *)
  type 'a queue = 'a list

  let empty = []

  let is_empty q = q = []

  let enqueue x q = q @ [ x ]

  let peek = function [] -> None | x :: _ -> Some x

  let dequeue = function [] -> None | _ :: q -> Some q
end

module TwoListQueue : Queue = struct
  (* [{front=[a;b]; back=[e;d;c]}] represents the queue
     containing the elements a,b,c,d,e. That is, the
     back of the queue is stored in reverse order.
     [{front; back}] is in *normal form* if [front]
     being empty implies [back] is also empty.
     All queues passed into or out of the module
     must be in normal form. *)
  type 'a queue = { front : 'a list; back : 'a list }

  let empty = { front = []; back = [] }

  let is_empty = function { front = []; back = [] } -> true | _ -> false

  (* Helper function to ensure that a queue is in normal form. *)
  let norm = function
    | { front = []; back } -> { front = List.rev back; back = [] }
    | q -> q

  let enqueue x q = norm { q with back = x :: q.back }

  let peek = function
    | { front = []; _ } -> None
    | { front = x :: _; _ } -> Some x

  let dequeue = function
    | { front = []; _ } -> None
    | { front = _ :: xs; back } -> Some (norm { front = xs; back })
end

module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings 
   * as [d] and also a mapping from [k] to [v], even if [k] was already 
   * mapped in [d]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (* [lookup k d] returns the value associated with [k] in [d].  
   * raises:  [Not_found] if [k] is not mapped to any value in [d]. *)
  val lookup : 'k -> ('k, 'v) t -> 'v
end

module BstDict : Dictionary = struct
  type 'a tree = Leaf | Node of 'a node

  and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let rec insert k v d =
    match d with
    | Leaf -> Node { value = (k, v); left = Leaf; right = Leaf }
    | Node { value; left; right } ->
        if fst value > k then Node { value; left = insert k v left; right }
        else if fst value < k then
          Node { value; left; right = insert k v right }
        else Node { value = (k, v); left; right }

  let rec lookup k d =
    match d with
    | Leaf -> raise Not_found
    | Node { value; left; right } ->
        if fst value > k then lookup k left
        else if fst value < k then lookup k right
        else snd value
end

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int

  val denominator : t -> int

  val to_string : t -> string

  val to_float : t -> float

  val add : t -> t -> t

  (* val mul : t -> t -> t *)
end

(* [gcd x y] is the greatest common divisor of [x] and [y].
 * requires: [x] and [y] are positive.
 *)
let rec gcd (x : int) (y : int) : int =
  if x = 0 then y else if x < y then gcd (y - x) x else gcd y (x - y)

module FractionImpl : Fraction = struct
  type t = int * int

  let make a b = (a, b)

  let numerator t = fst t

  let denominator t = snd t

  let to_string t =
    string_of_int (numerator t) ^ "/" ^ string_of_int (denominator t)

  let to_float t = float_of_int (numerator t) /. float_of_int (denominator t)

  let rec simplify t =
    if denominator t < 0 then simplify (make (-numerator t) (-denominator t))
    else
      let sign = if numerator t < 0 then -1 else 1 in
      let num = sign * numerator t and den = denominator t in
      let div = gcd num den in
      make (sign * num / div) (den / div)

  let add ta tb =
    let num = (numerator ta * denominator tb) + (numerator tb * denominator ta)
    and den = denominator ta * denominator tb in
    simplify (make num den)

  let mul ta tb =
    let num = numerator ta * numerator tb
    and den = denominator ta * denominator tb in
    simplify (make num den)
end

module CharMap = Map.Make (Char)

type date = { month : int; day : int }

module Date = struct
  type t = date

  let compare da db =
    match Stdlib.compare da.month db.month with
    | 0 -> Stdlib.compare da.day db.day
    | c -> c
end

module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let print_calendar =
  DateMap.iter (fun date event ->
      print_int date.month;
      print_string "/";
      print_int date.day;
      print_string (":\t" ^ event);
      print_newline ())

let is_for = CharMap.mapi (fun k v -> String.make 1 k ^ " is_for " ^ v)

let first_after calendar date =
  DateMap.find_first (fun k -> Date.compare k date > 0) calendar |> snd

module CisString = struct
  type t = string

  let compare da db = String.(compare (lowercase_ascii da) (lowercase_ascii db))
end

module CisSet = Set.Make (CisString)
