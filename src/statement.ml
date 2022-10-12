type t = {
  curr : string list; (** should be a stack*)
  equiv : (string * string) list;
}

type element =
    |Num of int
    |Add
    |Sub
    |Mul
    |Div

    