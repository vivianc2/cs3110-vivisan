type element =
  | Num of string
  | Opr of char

type t = element list
(** The abstract type represents the postfix expression that can be part of a
    user command. For example, [+ 3 2] is (2 + 3). The expression is recorded
    backward*)

exception Malformed
(** Raised when a malformed technique is parsed. *)

val infix_of_string : string -> t
(** [infix_of_stirng s] is the infix expression of string [s]*)

val exp_of_infix : t -> t
(** [exp_of_infix s] is the postfix expression of infix [s]*)

val string_of_exp : t -> string
(** [string_of_exp t] is the infix string of postfix expression [t]*)

val compare_op : char -> char -> bool
(** [compare_op c1 c2] is true if and only if c1 is an operation of lower
    priority than c2 *)

val compare_exp : t -> t -> bool
(** [compare t1 t2] is true if and only if t1 and t2 are the same expression*)