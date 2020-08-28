module OATable = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable deleted_size : int;
    mutable buckets : ('k * 'v) option option array;
  }

  let create n =
    {
      hash = Hashtbl.hash;
      size = 0;
      deleted_size = 0;
      buckets = Array.make n None;
    }

  let find_i tbl k =
    let n_buckets = Array.length tbl.buckets
    and h = tbl.hash k
    and i = ref 0
    and found = ref false in
    while
      !i < n_buckets
      &&
      let b = tbl.buckets.((h + !i) mod n_buckets) in
      match b with
      | None -> false
      | Some None -> true (* deleted *)
      | Some (Some (k2, v)) ->
          if k = k2 then (
            found := true;
            false )
          else true
    do
      i := !i + 1
    done;
    if !found then (h + !i) mod n_buckets else raise Not_found

  let find tbl k =
    match tbl.buckets.(find_i tbl k) with
    | Some (Some (k2, v)) ->
        assert (k = k2);
        v
    | _ -> raise (Failure "weird")

  let iter f tbl =
    for i = 0 to pred (Array.length tbl.buckets) do
      match tbl.buckets.(i) with Some (Some (k, v)) -> f k v | _ -> ()
    done

  let add tbl k v =
    let add_to_buckets buckets k v =
      let n = Array.length buckets in
      let h = tbl.hash k and i = ref 0 in
      (* Bug. currently actas as multitable *)
      while
        !i < n
        &&
        match buckets.((h + !i) mod n) with
        | None -> false
        | Some None -> true
        | Some (Some (k2, v)) -> k2 <> k
        (* if key already exists overwrite value *)
      do
        i := !i + 1
      done;
      if !i = n then raise Not_found
      else buckets.((h + !i) mod n) <- Some (Some (k, v))
    in
    if float_of_int (tbl.size + tbl.deleted_size + 1) > 1.0 /. 4.0 then (
      (* Double table size. Rehash all existing bindings. *)
      let new_buckets_size = 2 * Array.length tbl.buckets in
      let new_buckets = Array.make new_buckets_size None in
      iter (add_to_buckets new_buckets) tbl;
      tbl.buckets <- new_buckets;
      tbl.deleted_size <- 0 )
    else ();
    (try ignore (find_i tbl k) with Not_found -> tbl.size <- tbl.size + 1);
    add_to_buckets tbl.buckets k v

  let remove tbl k =
    tbl.buckets.(find_i tbl k) <- Some None;
    tbl.size <- tbl.size - 1;
    tbl.deleted_size <- tbl.deleted_size + 1
end

module type Comparable = sig
  type t

  val ( = ) : t -> t -> bool

  val ( < ) : t -> t -> bool

  val ( > ) : t -> t -> bool
end

module BstSet = struct
  type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf

  module Make (T : Comparable) = struct
    let empty : T.t tree = Leaf

    (** [mem x t] is [true] iff [x] is a member of [t]. *)
    let rec mem x = function
      | Leaf -> false
      | Node (y, l, r) -> T.( = ) x y || (T.( < ) x y && mem x l) || mem x r

    (** [insert x t] is [t] . *)
    let rec insert x = function
      | Leaf -> Node (x, Leaf, Leaf)
      | Node (y, l, r) as t ->
          if T.( = ) x y then t
          else if T.( < ) x y then Node (y, insert x l, r)
          else Node (y, l, insert x r)
  end
end
