(* Exercise: mutable fields [✭] *)

type student_gpa = { name : string; mutable gpa : float }

let alice = { name = "Alice"; gpa = 3.7 }

let _ = alice.gpa <- 4.0

(* Exercise: refs [✭] *)

let _ = ref true (* bool ref *)

let _ = ref [ 1 ] (* int list ref *)

let _ = [ ref 1 ] (* int ref list *)

(* Exercise: inc fun [✭] *)
let inc = ref (fun x -> x + 1)

let _ = !inc 3109

(* Exercise: addition assignment [✭✭] *)
let ( +:= ) x y = x := !x + y

let x = ref 0

let _ = x +:= 3110

let _ = !x

(* Exercise: physical equality [✭✭] *)
let x = ref 0

let y = x

let z = ref 0

let _ = x == y (* true *)

let _ = x == z (* false *)

let _ = x = y (* true *)

let _ = x = z (* true *)

let _ = x := 1 (* () *)

let _ = x = y (* true *)

let _ = x = z (* false *)

(* AF: the float array [| x1; ...; xn |] represents the 
 *     vector (x1, ..., xn) 
 * RI: the array is non-empty *)
type vector = float array

(* Exercise: norm [✭✭] *)

let norm (u : vector) : float =
  u |> Array.map (fun x -> x *. x) |> Array.fold_left ( +. ) 0.0 |> sqrt

(* Exercise: normalize [✭✭] *)

let normalize (u : vector) : unit =
  let n = norm u in
  Array.iteri (fun i x -> u.(i) <- x /. n) u

let a = [| 1.; 1. |]

let _ = normalize a

let _ = a

(* Exercise: normalize loop [✭✭] *)
let normalize_loop (u : vector) : unit =
  let n = norm u in
  for i = 0 to Array.length u - 1 do
    u.(i) <- u.(i) /. n
  done

(* Exercise: norm loop [✭✭] *)

let norm_loop (u : vector) : float =
  let n = ref 0.0 in
  for i = 0 to Array.length u - 1 do
    n := !n +. (u.(i) *. u.(i))
  done;
  sqrt !n

(* Exercise: init matrix [✭✭✭] *)

let init_matrix sx sy f =
  let res = Array.make sx [||] in
  for x = 0 to pred sx do
    Array.unsafe_set res x (Array.init sy (fun y -> f x y))
  done;
  res

let m = init_matrix 5 5 (fun x y -> x + y)
