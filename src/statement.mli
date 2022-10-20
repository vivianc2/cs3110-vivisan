type stm
(** The abstract type of values representing statements to prove. *)

exception NotMatch
exception NotReflexive
exception QED
exception NotZeroAddPattern
exception NotZeroMulPattern

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
    replaces x+0 as x, if the result is false, then raise NotZeroAddPattern *)

val mul_zero : stm -> stm
(** [mul_zero stm] takes in statement [stm], returns equivalent stm that
    replaces x*0 as 0+, if the result is false, then raise NotZeroAddPattern *)
