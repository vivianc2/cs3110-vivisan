type t
(** The abstract type of values representing statements to prove. *)

val make_stm : Expression.t * Expression.t -> (Expression.t * Expression.t) list -> t

val string_of_stm : t -> string
(** [string_of_stm t] is the string that [t] represents.*)

val is_valid : t -> bool
(** [is_valid t] is true if and only if the left and the right expression of 
    statement [t] are same.*)

val substitute : t -> string list -> t
(** [substitute t e] is the statement after changing replacing [e] with its equivalent form
    in [t]*)

val next_statement: t -> Technique.technique -> t
(** [next_statement t tech] is the statement after applying technique [tech] to [t] *)

