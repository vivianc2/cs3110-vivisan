(** Representation of statements to prove.

    This module represents the statements that the user will prove. It contains
    the methods to create one statement, or adjust the statement according to
    different theorems applied*)

type stm
(** The abstract type of values representing statements to prove. *)

exception NotMatch
(** Raised when an substitution fails. *)

exception NotReflexive
(** Raised when the reflexive technique fails. *)

exception QED
(** Raised when the proof is complete. *)

exception NotZeroAddPattern
(** Raised when the zero_add theorem could not be applied. *)

exception NotZeroMulPattern
(** Raised when the zero_mul theorem could not be applied. *)

exception NotAddZeroPattern
(** Raised when the add_zero theorem could not be applied. *)

exception NotMulZeroPattern
(** Raised when the mul_zero theorem could not be applied. *)

exception NotSuccPattern
(** Raised when the add_succ theorem could not be applied. *)

exception NotSuccEqPattern
(** Raised when the succ_eq theorem could not be applied. *)

val make_stm :
  Expression.t * Expression.t -> (Expression.t * Expression.t) list -> stm
(** [make_stm curr equiv] is the statement with current statements to proven set
    to [curr] and equivalence relation set to [equiv]*)

val get_curr : stm -> (Expression.t * Expression.t) list
(** [get_curr stm] is current statements to be proven*)

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
    or succ(a) + b + c = succ (a+b+c) with succ keyword represented as $. If the
    result is false, then raise NotAddSuccPattern *)

val succ_eq : stm -> stm
(** [succ_eq stm] returns the statement that replaces succ (n) with n+1 if n is
    a natural number or expression about naturla number. Succ is represented
    with char '$' *)
