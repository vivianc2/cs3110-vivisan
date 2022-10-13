type element =
    |Num of string
    |Opr of char

type t
(** The abstract type represents the postfix expression that can be part of a
    user command. For example, [+ 3 2] is (2 + 3). The expression is recorded backward*)

exception Malformed
(** Raised when a malformed technique is parsed. *)

val string_of_exp : t -> string
(** [string_of_exp t] is the infix string of postfix expression [t]*)

val compare_exp : t -> t -> bool
(** [compare t1 t2] is true if and only if t1 and t2 are the same expression*)