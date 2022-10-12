open Statement

type expression = string list
(** The type [expression] represents the expression that can be part of a
    user command. *)

type technique = 
    |Refl
    |Rw of expression
(** The type [technique] represents a user command that is decomposed into a
    verb and possibly an expression. Invariant: the [object_phrase] carried
    by [Go] must not be empty. *)

val parse : string -> technique
(** [parse str] parses a player's input into a [technique].*)



