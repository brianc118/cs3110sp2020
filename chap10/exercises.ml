(* Exercise: parse [✭] *)

(* Exercise: parser.ml and lexer.ml [✭] *)

(** Exercise: simpl ids [✭✭]
 * Only letters are allowed in id. Ocaml allows underscore and digits etc *)

(* Exercise: infer [✭] *)
let infer (s:string) : typ = typeof (interp_big s)
