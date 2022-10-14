(* type expression = string list (** The type [expression] represents the suffix
   expression that can be part of a user command. For example, [+ 3 2] is (2 +
   3). The expression is recorded backward*) *)

type technique =
  | Refl
  | Rw of string
      (** The type [technique] represents a user command that is decomposed into
          a verb and possibly an expression. Invariant: the [object_phrase]
          carried by [Go] must not be empty. *)

exception Empty
(** Raised when an empty technique is parsed. *)

exception Malformed
(** Raised when a malformed technique is parsed. *)

exception ShowHelp
(** Raised xxx. *)

exception Quit

val parse : string -> technique
(** [parse str] parses a player's input into a [technique].*)
