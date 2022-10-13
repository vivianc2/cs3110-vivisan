type stm
(** The abstract type of values representing statements to prove. *)

val make_stm :
  Expression.t * Expression.t -> (Expression.t * Expression.t) list -> stm

val string_of_stm : stm -> string
(** [string_of_stm stm] is the string that [stm] represents.*)

val is_valid : stm -> bool
(** [is_valid stm] is true if and only if the left and the right expression of
    statement [stm] are same.*)

val substitute : stm -> Expression.t -> stm
(** [substitute stm e] is the statement after changing replacing [e] with its
    equivalent form in [stm]*)

val next_statement : stm -> Technique.technique -> stm
(** [next_statement stm tech] is the statement after applying technique [tech]
    to [stm] *)
