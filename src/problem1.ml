(** Your solution for Problem 1 of Homework 7 *)

type ('a,'b) sum = 
  | L of 'a 
  | R of 'b

(** Write OCaml functions that satisfy the following polymorphic type signatures *)

(** f: ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)

(* let f ... = ... *)

(** g: ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)

(* let g ... = ... *)

(** h: ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)

(* let h ... = ... *)

(** i: ('a, 'b) sum * ('a -> 'c) * ('b -> 'c) -> 'c *)

(* let i ... = ... *)

(** j: ('a, 'b * 'c) sum -> ('a, 'b) sum * ('a, 'c) sum *)

(* let j ... = ... *)

