(** Representation of techniques in proofs.

    This module represents the list of techniques in proofs, and parsing methods
    that extract them from commands*)

(** The type [technique] represents a user command that is decomposed into a
    verb and possibly an expression. Invariant: the [object_phrase] carried by
    [Go] must not be empty. *)
type technique =
  | Refl
  | Rw of string
  | Ind of string list

exception Empty
(** Raised when an empty technique is parsed. *)

exception Malformed
(** Raised when a malformed technique is parsed. *)

exception ShowHelp
(** Raised when the user needs to show help, which are the valid operations and
    the equivalent expressions. *)

exception Quit
(** Raised when the user wants to quit the game. *)

exception Retry
(** Raised when the user wants to retry the current level of the game. *)

val parse : string -> technique
(** [parse str] parses a player's input into a [technique].*)
