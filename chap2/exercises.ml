(* Exercise: operators [✭✭] *)

let _ = 42 * 10

let _ = 3.14 /. 2.0

(* requires: n>=0 *)
(* returns: x to the power of n *)
let rec pow x n = if n = 0 then 1.0 else x *. pow x (n - 1)

let _ = pow 4.2 7

(* Exercise: equality [✭] *)

let _ = 42 = 42

let _ = "hi" = "hi"

let _ = "hi" == "hi"

(* Exercise: assert [✭] *)

let _ = assert true

(* assert falselet _ =  *)

let _ = assert (2110 <> 3110)

(* Exercise: if [✭] *)

let _ = if 2 > 1 then 42 else 7

(* Exercise: double fun [✭] *)
let double x = 2 * x

let _ = assert (double 1 = 2)

let _ = assert (double (-2) = -4)

(* Exercise: more fun [✭✭] *)
let cube x = x *. x *. x

let _ = assert (cube 1.0 = 1.0)

let _ = assert (cube (-2.0) = -8.0)

let sign n = if n = 0 then 0 else if n > 0 then 1 else -1

let _ = assert (sign (-5) = -1)

let _ = assert (sign 3 = 1)

let _ = assert (sign 0 = 0)

let circ_area r = Float.pi *. r *. r

let _ = assert (circ_area 2.0 = Float.pi *. 4.0)

(* Exercise: RMS [✭✭] *)
let rms x y = Float.sqrt (((x *. x) +. (y *. y)) /. 2.0)

let _ = assert (rms 3.0 4.0 = 3.53553390593273775)

(* Exercise: date fun [✭✭✭] *)
let is_date d m =
  d >= 1
  && ( d <= 31
       && ( m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug"
          || m = "Oct" || m = "Dec" )
     || (d <= 30 && (m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov"))
     || (d <= 28 && m = "Feb") )

let _ = assert (is_date 28 "Feb")

let _ = assert (not (is_date 29 "Feb"))

let _ = assert (is_date 31 "Jan")

let _ = assert (not (is_date 31 "Jun"))

(* Exercise: fib [✭✭] *)
let rec fib n = if n <= 2 then 1 else fib (n - 1) + fib (n - 2)

let _ = assert (fib 1 = 1)

let _ = assert (fib 2 = 1)

let _ = assert (fib 3 = 2)

let _ = assert (fib 4 = 3)

(* Exercise: fib fast [✭✭✭] *)
let rec h n pp p = if n = 1 then p else h (n - 1) p (pp + p)

let fib_fast n = h n 0 1

let _ = assert (fib_fast 1 = 1)

let _ = assert (fib_fast 2 = 1)

let _ = assert (fib_fast 3 = 2)

let _ = assert (fib_fast 50 = 12586269025)

let _ = fib_fast 91

(* Find n such that fib_fast n <= 0 *)
let rec fib_overflow n = if fib_fast n <= 0 then n else fib_overflow (n + 1)

(* Exercise: divide [✭✭] *)
let divide ~numerator:a ~denominator:b = a /. b

let _ = assert (divide 1.0 2.0 = 0.5)

(* Exercise: average [✭✭] *)
let ( +/. ) x y = (x +. y) /. 2.0

let _ =
  assert (1.0 +/. 2.0 = 1.5);
  assert (0. +/. 0. = 0.);

  (* Exercise: hello world [✭] *)
  print_endline "Hello world!"

let _ = print_string "Hello world!"
