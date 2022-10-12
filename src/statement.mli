type t
(** The abstract type of values representing statements to prove. *)

val to_string : t -> string
(** [to_string t] is the string that [t] represents.*)

val equal : t -> bool
(** [equal t] is true if and only if the left and the right part of statement [t] are same.*)

val substitute : t -> string list -> t
(** [substitute t e] is the statement after changing replacing [e] with its equivalent form
    in [t]*)

val next_statement: t -> Technique.technique -> t
(** [next_statement t tech] is the statement after applying technique [tech] to [t] *)

