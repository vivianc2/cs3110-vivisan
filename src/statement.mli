type stm
(** The abstract type of values representing statements to prove. *)

exception NotMatch
exception NotReflexive
exception QED
exception NotZeroAddPattern
exception NotZeroMulPattern
exception NotAddZeroPattern
exception NotMulZeroPattern

val make_stm :
  Expression.t * Expression.t -> (Expression.t * Expression.t) list -> stm

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

(* val find_zero : Expression.t -> Expression.t -> bool * Expression.t

   val find_add : Expression.t -> Expression.t -> Expression.t -> bool *
   Expression.t * Expression.t *)
