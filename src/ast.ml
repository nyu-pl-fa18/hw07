(** {0} Abstract syntax for core calculus of MiniML *)

open Util

(** {1} Type definitions for representing ASTs *)
  
(** Variable names *)
type var = string
      
(** Inbuilt functions *)
type inbuilt_fun =
  | Not (* not *)
  | Fix (* fix (fixpoint combinator) *)

(** Binary infix operators *)
type binop =
  | Mult  (* * *)
  | Div   (* / *)
  | Mod   (* mod *)
  | Plus  (* + *)
  | Minus (* - *)
  | Eq    (* = *)
  | Ne    (* <> *)
  | Lt    (* < *)
  | Gt    (* > *)
  | Le    (* <= *)
  | Ge    (* >= *)
  | And   (* && *)
  | Or    (* || *)

(** Terms *)
type term =
  | FunConst of inbuilt_fun * pos      (* f (inbuilt function) *)
  | IntConst of int * pos              (* i (int constant) *)
  | BoolConst of bool * pos            (* b (bool constant) *)
  | Var of var * pos                   (* x (variable) *)
  | App of term * term * pos           (* t1 t2 (function application) *)
  | BinOp of binop * term * term * pos (* t1 bop t2 (binary infix operator) *)
  | Ite of term * term * term * pos    (* if t1 then t2 else t3 (conditional) *)
  | Lambda of var * term * pos         (* fun x -> t (lambda abstraction) *)

(** Values *)
type value =
  | IntVal of int (* i *)
  | BoolVal of bool (* b *)
  | Closure of var * term (* fun x -> t *)
        

(** Pretty printing *)

(** Return the precedence of the top-level syntactic construct in a term [t]. 
  * Lower values mean higher precedence. *)
let precedence = function
  | FunConst _ | IntConst _ | BoolConst _ | Var _ -> 0
  | App _ -> 1
  | BinOp (bop, _, _, _) ->
      (match bop with
      | Mult | Div | Mod -> 3
      | Plus | Minus -> 4
      | Eq | Ne | Lt | Gt | Le | Ge -> 5
      | And -> 6
      | Or -> 7)
  | Ite _ -> 8
  | Lambda _ -> 9

(** check whether given binary operator is left-associative *)
let is_left_assoc = function
  | And | Or -> false
  | _ -> true

        
let string_of_value = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | Closure _ -> ":function"
        
let string_of_bop = function
  | Mult -> "*"
  | Div -> "\\"
  | Mod -> "mod"
  | Plus -> "+"
  | Minus -> "-"
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_inbuilt_fun = function
  | Fix -> "fix"
  | Not -> "not"

(** Part 1: implement the pretty printer using OCaml's Format module *)
let rec pr_term (ppf: Format.formatter) (t: term) : unit =
  failwith "Not yet implemented" (** TODO: replace this with your implementation *)

(** Convert the term [t] to its string representation *)
let string_of_term (t: term) : string =
  pr_term Format.str_formatter t;
  Format.flush_str_formatter ()

(** Pretty print the term [t] to output channel [out_ch]. *)
let print_term (out_ch: out_channel) (t: term) : unit =
  Format.fprintf
    (Format.formatter_of_out_channel out_ch) "%a@?" pr_term t

(** {1} Utility functions for manipulating ASTs *)

(** Extract the source code position associated with the given term. *)
let position_of_term = function
  | FunConst (_, pos) -> pos
  | IntConst (_, pos)
  | BoolConst (_, pos)
  | Var (_, pos)
  | App (_, _, pos)
  | BinOp (_, _, _, pos)
  | Ite (_, _, _, pos)
  | Lambda (_, _, pos) -> pos

(** Check whether terms [t1] and [t2] are syntactically equal modulo source code position tags. *)
let rec equal t1 t2 =
  match t1, t2 with
  | FunConst (f1, _), FunConst (f2, _) -> f1 = f2
  | IntConst (i1, _), IntConst (i2, _) -> i1 = i2
  | BoolConst (b1, _), BoolConst (b2, _) -> b1 = b2
  | Var (x1, _), Var(x2, _) -> x1 = x2
  | App (t11, t12, _), App (t21, t22, _) ->
      equal t11 t21 &&
      equal t12 t22
  | BinOp (bop1, t11, t12, _), BinOp (bop2, t21, t22, _) ->
      bop1 = bop2 &&
      equal t11 t21 &&
      equal t12 t22
  | Ite (t11, t12, t13, _), Ite (t21, t22, t23, _) ->
      equal t11 t21 &&
      equal t12 t22 &&
      equal t13 t23
  | Lambda (x1, t11, _), Lambda (x2, t21, _) ->
      x1 = x2 && equal t11 t21
  | _ -> false
        
(** Part 2: find the first free variable in term [t] if it exists and return it together with its position *)
let find_free_var (t: term) : (var * pos) option =
  None (** TODO: replace this with your implementation *)
    
(** Part 3: substitute all free occurrences of variable [x] in term [t] with term [s] (i.e. compute t[s/x]) *)
let subst (t: term) (x: var) (s: term) =
  failwith "Not yet implemented" (** TODO: replace this with your implementation *)
