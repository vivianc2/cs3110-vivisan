open Expression
open Technique

type stm = {
  curr : Expression.t * Expression.t;
  equiv : (Expression.t * Expression.t) list;
}

let make_stm x y = { curr = x; equiv = y }

let string_of_stm stm =
  (stm.curr |> fst |> string_of_exp) ^ "=" ^ (stm.curr |> snd |> string_of_exp)

let is_valid stm = compare_exp (stm.curr |> fst) (stm.curr |> snd)

exception NotMatch

let rec substitute_chunck lst e e_equiv =
  match (lst, e) with
  | a, [] -> e_equiv @ a
  | [], _ -> raise NotMatch
  | h :: t, eh :: et ->
      if h = eh then substitute_chunck t et e_equiv else raise NotMatch

let rec substitute_helper lst e e_equiv =
  match (lst, e) with
  | a, [] -> a
  | [], _ -> []
  | h :: t, eh :: et ->
      if h = eh then
        try substitute_chunck lst e e_equiv
        with NotMatch -> h :: substitute_helper t e e_equiv
      else h :: substitute_helper t e e_equiv

(** [substitute stm e] is the statement after changing replacing [e] with its
    equivalent form in [stm]*)
let substitute stm e =
  let _, e_equiv = List.find (fun (x, _) -> compare_exp x e) stm.equiv in
  {
    stm with
    curr =
      ( substitute_helper (fst stm.curr) e e_equiv,
        substitute_helper (snd stm.curr) e e_equiv );
  }

exception NotReflexive
exception QED

(** [next_statement stm tech] is the statement after applying technique [tech]
    to [stm] *)
let next_statement stm tech =
  match tech with
  | Refl ->
      if compare_exp (fst stm.curr) (snd stm.curr) then raise QED
      else raise NotReflexive
  | Rw str -> substitute stm (str |> infix_of_string |> exp_of_infix)
