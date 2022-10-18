open Expression
open Technique

type stm = {
  curr : t * t;
  equiv : (t * t) list;
}

let make_stm x y = { curr = x; equiv = y }

let string_of_stm stm =
  (stm.curr |> fst |> string_of_exp) ^ " = " ^ (stm.curr |> snd |> string_of_exp)

let string_of_equiv stm =
  stm.equiv
  |> List.map (fun x ->
         (x |> fst |> string_of_exp)
         ^ " = "
         ^ (x |> snd |> string_of_exp)
         ^ "; ")
  |> List.fold_left ( ^ ) ""

let is_valid stm = compare_exp (stm.curr |> fst) (stm.curr |> snd)

exception NotMatch

let rec substitute_chunk lst exp equiv_exp =
  match (lst, exp) with
  | a, [] -> equiv_exp @ a
  | [], _ -> raise NotMatch
  | h :: t, eh :: et ->
      if h = eh then substitute_chunk t et equiv_exp else raise NotMatch

let rec substitute_helper lst exp equiv_exp =
  match (lst, exp) with
  | l, [] -> l
  | [], _ -> []
  | h :: t, eh :: et ->
      if h = eh then
        try substitute_chunk lst exp equiv_exp
        with NotMatch -> h :: substitute_helper t exp equiv_exp
      else h :: substitute_helper t exp equiv_exp

let substitute stm exp =
  let sub_stm stm equiv_exp =
    {
      stm with
      curr =
        ( substitute_helper (fst stm.curr) exp equiv_exp,
          substitute_helper (snd stm.curr) exp equiv_exp );
    }
  in
  match List.find (fun (x, _) -> compare_exp x exp) stm.equiv with
  | _, equiv_exp -> sub_stm stm equiv_exp
  | exception _ -> (
      match List.find (fun (_, y) -> compare_exp y exp) stm.equiv with
      | equiv_exp, _ -> sub_stm stm equiv_exp
      | exception _ -> raise NotMatch)

exception NotReflexive
exception QED

let next_statement stm tech =
  match tech with
  | Refl ->
      if compare_exp (fst stm.curr) (snd stm.curr) then raise QED
      else raise NotReflexive
  | Rw str -> substitute stm (str |> exp_of_string)
