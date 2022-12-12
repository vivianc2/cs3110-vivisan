(** Representation of mathematical expressions.

    This module represents the expressions that the user could apply in their
    proof. It contains the methods to convert user input to expressions.*)

(** The element in expressions, which is represented by string if the element is
    a number or a variable, and a char if the element is an operation*)
type element =
  | Num of string
  | Opr of char

type t = element list
(** The abstract type represents the postfix expression that can be part of a
    user command. For example, [+ 3 2] is (2 + 3). The expression is recorded
    backward*)

exception Malformed
(** Raised when a malformed technique is used. *)

val infix_of_string : string -> t
(** [infix_of_stirng s] is the infix expression of string [s]*)

val exp_of_infix : t -> t
(** [exp_of_infix s] is the postfix expression of infix [s]*)

val exp_of_string : string -> t
(** [infix_of_stirng s] is the postfix expression of string [s]*)

val string_of_exp : t -> string
(** [string_of_exp t] is the infix string of postfix expression [t]*)

val compare_op : char -> char -> bool
(** [compare_op c1 c2] is true if and only if c1 is an operation of lower
    priority than c2. Precondition: the only operations allowed to pass in is
    '+' | '-' | '*' | '/' | '(' | ')' *)

val compare_exp : t -> t -> bool
(** [compare t1 t2] is true if and only if t1 and t2 are the same expression*)