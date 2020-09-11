type 'a stream = Cons of 'a * (unit -> 'a stream)

(* Exercise: pow2 [✭✭] *)

let pow2 : int stream =
  let rec pow2i (n : int) : int stream = Cons (n, fun () -> pow2i (2 * n)) in
  pow2i 1

(* Exercise: more streams [✭✭, optional] *)

let enat : int stream =
  let rec enati (n : int) : int stream =
    Cons (2 * n, fun () -> enati (n + 1))
  in
  enati 0

let lalph : char stream =
  let a =
    Array.make 26 'a'
    |> Array.mapi (fun i c -> int_of_char c + i |> char_of_int)
  in
  let rec lalphi (n : int) : char stream =
    Cons (a.(n), fun () -> lalphi ((n + 1) mod 26))
  in
  lalphi 0

type flip = Heads | Tails

let rec rand_flips : flip stream =
  Cons ((if Random.bool () then Heads else Tails), fun () -> rand_flips)

(* Exercise: nth [✭✭] *)

let rec nth (s : 'a stream) (n : int) : 'a =
  match s with Cons (x, t) -> if n = 0 then x else nth (t ()) (n - 1)

let () = assert (nth pow2 0 = 1)

let () = assert (nth pow2 4 = 16)

let () = assert (nth enat 0 = 0)

let () = assert (nth enat 2 = 4)

let () = assert (nth lalph 2 = 'c')

let () = assert (nth lalph 27 = 'b')

(* Exercise: hd tl [✭✭] *)

(* Exercise: filter [✭✭✭] *)

let rec filter (p : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with
  | Cons (x, t) ->
      if p x then Cons (x, fun () -> filter p (t ())) else filter p (t ())

(* Exercise: interleave [✭✭✭] *)

let rec interleave (s1 : 'a stream) (s2 : 'a stream) =
  match s1 with Cons (x, t) -> Cons (x, fun () -> interleave s2 (t ()))

let () = assert (nth (interleave enat pow2) 1 = 1)

let () = assert (nth (interleave enat pow2) 20 = 20)

let () = assert (nth (interleave enat pow2) 5 = 4)

(* Exercise: sift [✭✭✭] *)

let sift (n : int) (s : int stream) : int stream =
  filter (fun x -> x mod n <> 0) s

(* Exercise: primes [✭✭✭] *)

let primes : int stream =
  let rec helper (s : int stream) : int stream =
    match s with Cons (x, t) -> Cons (x, fun () -> helper (sift x (t ())))
  and nat2 : int stream =
    let rec nat2i (n : int) : int stream = Cons (n, fun () -> nat2i (n + 1)) in
    nat2i 2
  in
  helper nat2

let () = assert (nth primes 0 = 2)

let () = assert (nth primes 3 = 7)

let () = assert (nth primes 100 = 547)

(* Exercise: approximately e [✭✭✭✭] *)

let e_terms (x : float) : float stream =
  let rec e_term (k : float) (i : int) : float stream =
    Cons (k, fun () -> e_term (k *. x /. float_of_int i) (i + 1))
  in
  e_term 1.0 1

let _ = Array.make 10 0.0 |> Array.mapi (fun i v -> nth (e_terms 2.0) i)

let total (s : float stream) : float stream =
  let rec acc (c : float) (s : float stream) : float stream =
    match s with Cons (x, t) -> Cons (c +. x, fun () -> acc (c +. x) (t ()))
  in
  acc 0.0 s

let rec within (eps : float) (s : float stream) : float =
  let rec r (pre : float) (s : float stream) : float =
    match s with
    | Cons (x, t) -> if abs_float (x -. pre) < eps then x else r x (t ())
  in
  match s with Cons (x, t) -> r x (t ())

let e (x : float) (eps : float) : float = e_terms x |> total |> within eps

(* Exercise: better e [✭✭✭✭, advanced] *)

let rec within_better (eps : float) (s : float stream) : float =
  let rec r (pre : float) (s : float stream) : float =
    match s with
    | Cons (x, t) ->
        if
          abs_float (x -. pre) /. (((abs_float x +. abs_float pre) /. 2.) +. 1.)
          < eps
        then x
        else r x (t ())
  in
  match s with Cons (x, t) -> r x (t ())

let e (x : float) (eps : float) : float =
  e_terms x |> total |> within_better eps

(* Exercise: different stream rep [✭✭✭] *)

(* it's even lazier in that the first element isn't loaded either *)

(* Exercise: lazy hello [✭] *)

let lazy_hello = lazy (print_endline "Hello lazy world")

let _ = Lazy.force lazy_hello

(* Exercise: lazy and [✭✭] *)

let ( &&& ) (lb1 : bool Lazy.t) (lb2 : bool Lazy.t) : bool =
  if Lazy.force lb1 then Lazy.force lb2 else false

(* Exercise: lazy stream [✭✭✭] *)

type 'a lazystream = Cons of 'a * 'a lazystream Lazy.t

let rec map (f : 'a -> 'b) (s : 'a lazystream) =
  match s with Cons (x, t) -> Cons (f x, lazy (map f (Lazy.force t)))

let rec filter (p : 'a -> bool) (s : 'a lazystream) =
  match s with
  | Cons (x, t) ->
      if p x then Cons (x, lazy (filter p (Lazy.force t)))
      else filter p (Lazy.force t)

(* Exercise: promise and resolve [✭✭] *)

open Promises

let (p : int Promise.promise), r = Promise.make ()

let _ =
  Promise.( >>= ) p (fun x ->
      print_int x;
      print_newline ();
      Promise.return ())

let _ = print_endline "Before promise is resolved."

let _ = Promise.resolve r 314159

let _ = print_endline "After promise is resolved."

(* Exercise: promise and resolve lwt [✭✭] *)

open Lwt

let (p : int Lwt.t), r = Lwt.wait ()

let _ =
  p >>= fun x ->
  print_int x;
  print_newline ();
  Lwt.return ()

let _ = print_endline "Before promise is resolved."

let _ = Lwt.wakeup r 2718

let _ = print_endline "After promise is resolved."

(* Exercise: timing challenge 1 [✭✭] *)

open Lwt_io

(** [delay s] is a promise that resolves after about [s] seconds. *)
let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec

let delay_then_print () = delay 3. >>= fun () -> Lwt_io.printl "done"

(* Exercise: timing challenge 2 [✭✭✭] *)

open Lwt.Infix

let timing2 () =
  let _t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
  let _t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
  let _t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
  Lwt_io.printl "all done"

(* Exercise: timing challenge 3 [✭✭✭] *)

let timing3 () =
  delay 1. >>= fun () ->
  Lwt_io.printl "1" >>= fun () ->
  delay 10. >>= fun () ->
  Lwt_io.printl "2" >>= fun () ->
  delay 20. >>= fun () ->
  Lwt_io.printl "3" >>= fun () -> Lwt_io.printl "all done"

(* Exercise: timing challenge 4 [✭✭✭] *)

let timing4 () =
  let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
  let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
  let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
  Lwt.join [ t1; t2; t3 ] >>= fun () -> Lwt_io.printl "all done"

(* let _ = Lwt_main.run (delay_then_print () >>= timing2 >>= timing3 >>= timing4) *)

(* Exercise: add opt [✭✭] *)

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with Some x -> f x | None -> None
end

let add : int Maybe.t -> int Maybe.t -> int Maybe.t =
 fun ma mb ->
  Maybe.(
    ma >>= fun a ->
    mb >>= fun b -> return (a + b))

let () = assert (Maybe.(add (return 2) (return 3) = return 5))

(* Exercise: fmap and join [✭✭] *)

module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val join : 'a t t -> 'a t
end

module ExtMaybe : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with Some x -> f x | None -> None

  let ( >>| ) m f = match m with Some x -> return (f x) | None -> None

  let join = function Some m -> m | None -> None
end

(* Exercise: fmap and join again [✭✭] *)

module ExtMaybeAgain : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f = match m with Some x -> f x | None -> None

  let ( >>| ) m f = m >>= fun x -> return (f x)

  let join m = m >>= fun m -> m
end

(* Exercise: bind from fmap+join [✭✭✭] *)

module type FmapJoinMonad = sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val join : 'a t t -> 'a t

  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  type 'a t = 'a M.t

  let return = M.return

  let ( >>= ) m f = M.( >>| ) m f |> M.join
end

(* Exercise: list monad [✭✭✭] *)

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x = [ x ]

  let ( >>| ) m f = List.map f m

  let join m = List.concat m

  let ( >>= ) m f = m >>| f |> join
end

(* Exercise: trivial monad laws [✭✭✭] *)

module Trivial : Monad = struct
  type 'a t = Wrap of 'a

  let return x = Wrap x

  let ( >>= ) (Wrap x) f = f x
end
