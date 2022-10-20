open Expression
open Technique

type stm = {
  curr : (t * t) list;
  equiv : (t * t) list;
}

let make_stm x y = { curr = [ x ]; equiv = y }

let print_explist explst =
  explst
  |> List.map (fun x ->
         (x |> fst |> string_of_exp)
         ^ " = "
         ^ (x |> snd |> string_of_exp)
         ^ "; ")
  |> List.fold_left ( ^ ) ""

let string_of_stm stm = print_explist stm.curr
let string_of_equiv stm = print_explist stm.equiv
let is_empty stm = stm.curr = []

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
    match stm.curr with
    |[] -> stm
    |h :: t-> {stm with
      curr =
        ( substitute_helper (fst h) exp equiv_exp,
          substitute_helper (snd h) exp equiv_exp ) :: t;
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
  match stm.curr with
  | [] -> raise QED
  | h :: t -> begin
      match tech with
      | Refl ->
          if compare_exp (fst h) (snd h) then 
            if t = [] then raise QED
            else {stm with curr = t}
          else raise NotReflexive
      | Rw str -> substitute stm (str |> exp_of_string)
      | Ind strlst -> raise QED
    end
