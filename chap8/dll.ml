(* An ['a node] is a node of a mutable doubly-linked list.
 * It contains a value of type ['a] and optionally has 
 * pointers to previous and/or next nodes. *)
type 'a node = {
  mutable prev : 'a node option;
  mutable next : 'a node option;
  value : 'a;
}

(* An ['a dlist] is a mutable doubly-linked list with elements 
 * of type ['a].  It is possible to access the first and 
 * last elements in constant time.  
 * RI: The list does not contain any cycles. *)
type 'a dlist = {
  mutable first : 'a node option;
  mutable last : 'a node option;
}

(* [create_node v] is a node containing value [v] with
 * no links to other nodes. *)
let create_node v = { prev = None; next = None; value = v }

(* [empty_dlist ()] is an empty doubly-linked list. *)
let empty_dlist () = { first = None; last = None }

(* [create_dlist n] is a doubly-linked list containing
 * exactly one node, [n]. *)
let create_dlist (n : 'a node) : 'a dlist = { first = Some n; last = Some n }

(* [insert_first d n] mutates dlist [d] by
 * inserting node [n] as the first node. *)
let insert_first (d : 'a dlist) (n : 'a node) : unit =
  match d.first with
  | None ->
      d.first <- Some n;
      d.last <- Some n
  | Some f ->
      f.prev <- Some n;
      n.next <- Some f;
      d.first <- Some n

(* [insert_last d n] mutates dlist [d] by
 * inserting node [n] as the last node. *)
let insert_last (d : 'a dlist) (n : 'a node) : unit =
  match d.last with
  | None ->
      d.first <- Some n;
      d.last <- Some n
  | Some l ->
      l.next <- Some n;
      n.prev <- Some l;
      d.last <- Some n

(* [insert_after d n1 n2] mutates dlist [d] by
 * inserting node [n2] after node [n1]. *)
let insert_after (d : 'a dlist) (n1 : 'a node) (n2 : 'a node) : unit =
  let opt_n = n1.next in
  n1.next <- Some n2;
  n2.prev <- Some n1;
  n2.next <- opt_n;
  match opt_n with
  | None -> ()
  | Some n ->
      n.prev <- Some n2;
      if d.last = Some n1 then d.last <- Some n2

(* [insert_before d n1 n2] mutates dlist [d] by
 * inserting node [n2] before node [n1]. *)
let insert_before (d : 'a dlist) (n1 : 'a node) (n2 : 'a node) : unit =
  let opt_n = n1.prev in
  n1.prev <- Some n2;
  n2.next <- Some n1;
  n2.prev <- opt_n;
  match opt_n with
  | None -> ()
  | Some n ->
      n.next <- Some n2;
      if d.first = Some n1 then d.first <- Some n2

(* [remove d n] mutates dlist [d] by removing node [n].
 * requires: [n] is a node of [d]. *)
let remove (d : 'a dlist) (n : 'a node) : unit =
  let opt_prev = n.prev and opt_next = n.next in
  if d.first = Some n then d.first <- opt_next;
  if d.last = Some n then d.last <- opt_prev;
  match opt_prev with
  | None -> ()
  | Some n -> (
      n.next <- opt_next;
      match opt_next with None -> () | Some n -> n.prev <- opt_prev )

(* [iter_forward d f] on a dlist [d] which has 
 * elements n1; n2; ... is (f n1); (f n2); ... *)
let iter_forward (d : 'a dlist) (f : 'a -> unit) : unit =
  let cur_opt = ref d.first in
  while
    match !cur_opt with
    | None -> false
    | Some n ->
        f n.value;
        cur_opt := n.next;
        true
  do
    ()
  done

(* [iter_backward d f] on a dlist [d] which has 
 * elements n1; n2; ... is ...; (f n2); (f n1) *)
let iter_backward (d : 'a dlist) (f : 'a -> unit) : unit =
  let cur_opt = ref d.last in
  while
    match !cur_opt with
    | None -> false
    | Some n ->
        f n.value;
        cur_opt := n.prev;
        true
  do
    ()
  done
