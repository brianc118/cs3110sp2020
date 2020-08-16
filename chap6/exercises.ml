(* Exercise: poly spec [✭✭✭] *)

(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  type t
  (** [t] is the type of polynomials *)

  val create : int list -> t
  (** [create c] is a polynomial of coefficients specified by c. *)

  val add : t -> t -> t
  (** [add p1 p2] is the resulting polynomial of adding polynomials p1 and p2. *)

  val eval : int -> t -> int
  (** [eval x p] is [p] evaluated at [x].  
      Example:  if [p] represents $3x^3 + x^2 + x$, then 
      [eval 10 p] is [3110]. *)

  val coeff : int -> t -> int
  (** [coeff k p] is the coefficient of x^k in the polynomial [p] . Requires: x >= 0 *)
end

module PolyImpl = struct
  (* Abstraction function: the list [c1; ...; cn] represents the polynomial
   * $c_1 + ... + c_n x^n$. [] represents zero. *)
  type t = int list

  let create c = c

  let rec add p1 p2 =
    match (p1, p2) with
    | [], [] -> []
    | [], b -> b
    | a, [] -> a
    | ah :: at, bh :: bt -> (ah + bh) :: add at bt

  let rec eval x p = match p with [] -> 0 | h :: t -> h + (x * eval x t)
end

(* Exercise: int set rep [✭✭✭] *)

(* Exercise: interval arithmetic [✭✭✭✭] *)
module type Interval = sig
  (* [t] is the type of an interval *)
  type t

  (* [rep_ok i] tests if [i] is a valid representation of an interval. *)
  val rep_ok : t -> t

  val empty : t

  (* [create a b] is an interval [a, b]. *)
  val create : float -> float -> t

  (* [equals i1 i2] is true iff interval i1 is equal to interval i2. *)
  val equals : t -> t -> bool

  (* [intersect i1 i2] is the intersection of the intervals i1 and i2. *)
  val intersect : t -> t -> t

  (* [union i1 i2] is the union of the intervals i1 and i2. *)
  val union : t -> t -> t

  val less_than : t -> t -> bool

  val width : t -> float

  val ( +. ) : t -> t -> t

  val ( -. ) : t -> t -> t

  val ( *. ) : t -> t -> t

  val ( /. ) : t -> t -> t

  val to_string : t -> string
end

module IntervalImpl : Interval = struct
  type t = (float * float) option

  let rep_ok = function
    | None -> None
    | Some t -> if fst t > snd t then raise (Failure "rep_not_ok") else Some t

  let empty = None

  let create a b = Some (a, b)

  let equals t1 t2 =
    match (t1, t2) with
    | None, None -> true
    | Some t1, Some t2 -> fst t1 = fst t2 && snd t1 = snd t2
    | _, _ -> false

  let intersect t1 t2 =
    match (t1, t2) with
    | Some t1, Some t2 ->
        let l = max (fst t1) (fst t2) and u = min (snd t1) (snd t2) in
        if l > u then None else Some (l, u)
    | _, _ -> None

  let union t1 t2 =
    match (t1, t2) with
    | Some t1, Some t2 ->
        let l = min (fst t1) (fst t2) and u = max (snd t1) (snd t2) in
        Some (l, u)
    | _, _ -> None

  let less_than t1 t2 =
    match (t1, t2) with Some t1, Some t2 -> snd t1 < fst t2 | _, _ -> true

  let width = function None -> 0.0 | Some t -> snd t -. fst t

  let apply op t1 t2 =
    match (t1, t2) with
    | Some t1, Some t2 -> Some (op (fst t1) (fst t2), op (snd t1) (snd t2))
    | _, _ -> raise (Failure "Invalid op")

  let ( +. ) = apply ( +. )

  let ( -. ) = apply ( -. )

  let ( *. ) = apply ( *. )

  let ( /. ) = apply ( /. )

  let to_string = function
    | None -> "empty"
    | Some t ->
        "[" ^ string_of_float (fst t) ^ ", " ^ string_of_float (snd t) ^ "]"
end

(* Exercise: association list maps [✭✭✭] *)
module type MyMap = sig
  type ('k, 'v) t
  (** [('k,'v) t] is the type of a map containing bindings 
      from keys of type ['k] to values of type ['v]. *)

  val empty : ('k, 'v) t
  (** [empty] is the map containing no bindings. *)

  val mem : 'k -> ('k, 'v) t -> bool
  (** [mem k m] is true if [k] is bound in [m] and false otherwise. *)

  val find : 'k -> ('k, 'v) t -> 'v
  (** [find k m] is [v] iff [k] is bound to [v] in [m]. 
      Raises: [Not_found] if [k] is not bound in [m]. *)

  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [add k v m] is the map [m'] that contains the same bindings
      as [m], and additionally binds [k] to [v]. If [k] was
      already bound in [m], its old binding is replaced by
      the new binding in [m']. *)

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  (** [remove k m] is the map [m'] that contains the same bindings
      as [m], except that [k] is unbound in [m']. *)
end

module MyMapImplA : MyMap = struct
  (* Abstraction function: the list [(k1, v1); ...; (kn, vn)] represents a map
     with bindings ki to vi. If ki = kj where i < j then the mapping of ki is vi not vj.
     Representation invariant: None *)
  type ('k, 'v) t = ('k * 'v) list

  let empty = []

  let mem k m = List.find_opt (fun p -> fst p = k) m <> None

  let find k m =
    if mem k m then List.find (fun p -> fst p = k) m |> snd else raise Not_found

  let add k v m = (k, v) :: m

  let remove k m = List.filter (fun p -> fst p <> k) m
end

module MyMapImplB : MyMap = struct
  (* Abstraction function: the list [(k1, v1); ...; (kn, vn)] represents a map
     with bindings ki to vi.
     Representation invariant: ki != kj for all i != j *)
  type ('k, 'v) t = ('k * 'v) list

  let empty = []

  let mem k m = List.find_opt (fun p -> fst p = k) m <> None

  let find k m =
    if mem k m then List.find (fun p -> fst p = k) m |> snd else raise Not_found

  let remove k m = List.filter (fun p -> fst p <> k) m

  let rec add k v m = if mem k m then m |> remove k |> add k v else (k, v) :: m
end

(* Exercise: function maps [✭✭✭✭] *)

module MyMapImplC : MyMap = struct
  type ('k, 'v) t = 'k -> 'v

  let empty _ = raise Not_found

  let mem k m = try m k |> ignore; true with _ -> false

  let find k m = m k

  let add k v m = fun x -> if x = k then v else m x

  let remove k m = fun x -> if x = k then raise Not_found else m x
end

(* Exercise: test with rep ok [✭✭✭] *)
open Queues

let q = TwoListQueue.empty

let q = TwoListQueue.singleton 1

let q = TwoListQueue.enqueue 2 q

let q = match TwoListQueue.dequeue q with
  | Some qq -> qq
  | None -> q