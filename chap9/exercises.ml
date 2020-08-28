(* Exercise: hash insert [✭✭] *)

(* Exercise: relax bucket RI [✭✭] *)

(* Exercise: strengthen bucket RI [✭✭] *)

(* Exercise: hash values [✭✭] *)

let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

module SI = Set.Make (Int)

let n =
  let s = SI.empty and i = ref 0 and lst = ref [ 0 ] and res = ref (-1) in
  while !i < 1000 && !res = -1 do
    if SI.find_opt (List.rev !lst |> Hashtbl.hash) s <> None then res := !i
    else (
      i := !i + 1;
      lst := !i :: !lst )
  done;
  res

(* Exercise: hashtbl usage [✭✭] *)
let htb = Hashtbl.create 16

let _ = List.map (fun x -> Hashtbl.add htb x (string_of_int x)) (1 -- 31)

let _ = assert (Hashtbl.find htb 1 = "1")

let _ = assert (Hashtbl.find_opt htb 0 = None)

(* Exercise: hashtbl bindings [✭✭] *)
let bindings tbl =
  Hashtbl.fold
    (fun k v _ ->
      print_int k;
      print_string (" -> " ^ v);
      print_newline ())
    tbl ()

let _ = bindings htb

(* Exercise: hashtbl stats [✭] *)
let _ = Hashtbl.stats htb

(* Exercise: hashtbl load factor [✭✭] *)
let load_factor tbl =
  let stats = Hashtbl.stats tbl in
  float_of_int stats.num_bindings /. float_of_int stats.num_buckets

let _ = load_factor htb

(* Exercise: functorial interface [✭✭✭] *)

module CITable = Hashtbl.Make (struct
  type t = string

  let equal i j = String.lowercase_ascii i = String.lowercase_ascii j

  let hash i = Hashtbl.hash (String.lowercase_ascii i)
end)

let htb = CITable.create 16

let _ = CITable.add htb "HI" 1

let _ = CITable.add htb "hI" 2

let _ = assert (CITable.find htb "hi" = 2)

(* Exercise: equals and hash [✭✭] *)

(* If two objects are equal but have different hashes, then finding keys in a
 * hash table using the hash won't work. e.g. Object A is a key. Object B = A.
 * h A != h B. So when finding B in the hash table, h B might direct us to a
 * different bucket than that of A, resulting in A not being found. Without
 * this property, the hash function is basically useless. The random int
 * function would qualify as a hash by this definition. *)

(* Exercise: bad hash [✭✭] *)
module BadTable = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash i = 0
end)

let htb = BadTable.create 16

let _ =
  List.fold_left (fun acc x -> BadTable.add htb (string_of_int x) x) () (1 -- 31)

let _ = BadTable.stats htb

(* Exercise: linear probing [✭✭✭✭] *)
open Modules

let htb = OATable.create 16

let _ = OATable.add htb "1" "one"

let _ = OATable.add htb "1" "unus"

let _ = htb

let _ = assert (OATable.find htb "1" = "unus")

let _ =
  try
    ignore (OATable.find htb "2");
    raise (Assert_failure ("Found key 2 but shouldn't exist", __LINE__, 0))
  with Not_found -> ()

let _ = OATable.add htb "2" "duo"

let _ = assert (OATable.find htb "2" = "duo")

let _ = htb

let _ = assert (htb.size = 2 && htb.deleted_size = 0)

let _ = OATable.remove htb "1"

let _ = assert (htb.size = 1 && htb.deleted_size = 1)

let _ =
  try
    ignore (OATable.find htb "1");
    raise (Assert_failure ("Found key 1 but shouldn't exist", __LINE__, 0))
  with Not_found -> ()

let _ =
  try
    OATable.remove htb "1";
    raise
      (Assert_failure
         ("Was able to remove key 1 but shouldn't exist", __LINE__, 0))
  with Not_found -> ()

let _ = OATable.remove htb "2"

let _ = assert (htb.size = 0 && htb.deleted_size = 2)

(* Exercise: functorized BST [✭✭✭] *)

module IntComparable = struct
  type t = int

  let ( = ) a b = a = b

  let ( < ) a b = a < b

  let ( > ) a b = a > b
end

module IntSet = BstSet.Make (IntComparable)

let int_set = IntSet.(empty |> insert 3 |> insert 1 |> insert 4)

(* Exercise: efficient traversal [✭✭✭] *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder ?(acc = []) = function
  | Leaf -> acc
  | Node (l, v, r) -> v :: preorder l ~acc:(preorder r ~acc)

let rec inorder ?(acc = []) = function
  | Leaf -> acc
  | Node (l, v, r) -> inorder l ~acc:(v :: inorder r ~acc)

let rec postorder ?(acc = []) = function
  | Leaf -> acc
  | Node (l, v, r) -> postorder l ~acc:(postorder r ~acc:(v :: acc))

let t =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

(* 
  t is
        4
      /   \
     2     6
    / \   / \
   1   3 5   7
*)

let () = assert (preorder t = [ 4; 2; 1; 3; 6; 5; 7 ])

let () = assert (inorder t = [ 1; 2; 3; 4; 5; 6; 7 ])

let () = assert (postorder t = [ 1; 3; 2; 5; 7; 6; 4 ])

(* Exercise: standard library set [✭✭, optional] *)
