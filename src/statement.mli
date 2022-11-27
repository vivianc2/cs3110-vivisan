type stm
(** The abstract type of values representing statements to prove. *)

exception NotMatch
exception NotReflexive
exception QED
exception NotZeroAddPattern
exception NotZeroMulPattern
exception NotAddZeroPattern
exception NotMulZeroPattern
exception NotSuccPattern
exception NotSuccEqPattern

val make_stm :
  Expression.t * Expression.t -> (Expression.t * Expression.t) list -> stm

val get_curr : stm -> (Expression.t * Expression.t) list

val string_of_stm : stm -> string
(** [string_of_stm stm] is the string that [stm] represents.*)

val string_of_equiv : stm -> string
(** [string_of_equiv stm] is the string that represents the equivalent
    expressions in [stm].*)

val is_empty : stm -> bool
(** [is_empty stm] is true if and only if the [stm.curr] is empty*)

val substitute : stm -> Expression.t -> stm
(** [substitute stm e] is the statement after changing replacing [e] with its
    equivalent form in [stm]*)

val next_statement : stm -> Technique.technique -> stm
(** [next_statement stm tech] is the statement after applying technique [tech]
    to [stm] *)

val add_zero : stm -> stm
(** [add_zero stm] takes in statement [stm], returns equivalent stm that
    replaces x+0 as x, if the result is false, then raise NotAddZeroPattern *)

val zero_add : stm -> stm
(** [zero_add stm] takes in statement [stm], returns equivalent stm that
    replaces 0+x as x, if the result is false, then raise NotZeroAddPattern *)

val mul_zero : stm -> stm
(** [mul_zero stm] takes in statement [stm], returns equivalent stm that
    replaces x*0 + y as 0+y or replaces x*0 with 0, if the result is false, then
    raise NotMulZeroPattern *)

val zero_mul : stm -> stm
(** [zero_mul stm] takes in statement [stm], returns equivalent stm that
    replaces 0*x + y as 0+y or replaces 0*x with 0, if the result is false, then
    raise NotZeroMulPattern *)

val add_succ : stm -> stm
(** [add_succ stm] returns equivalent stm that replaces a+succ(b) as succ (a+b)
    with succ keyword represented as $. If the result is false, then raise
    NotAddSuccPattern *)

val succ_add : stm -> stm
(** [succ_add stm] returns equivalent stm that replaces succ(a)+b as succ (a+b)
    with succ keyword represented as $. If the result is false, then raise
    NotAddSuccPattern *)

val succ_eq : stm -> stm
(** [succ_eq stm] returns the statement that replaces succ (n) with n+1 if n is
    a natural number or expression about naturla number. Succ is represented
    with char '$' *)

(* val find_zero : Expression.t -> Expression.t -> bool * Expression.t

   val find_add : Expression.t -> Expression.t -> Expression.t -> bool *
   Expression.t * Expression.t *)
