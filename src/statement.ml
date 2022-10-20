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
exception NotZeroAddPattern
exception NotZeroMulPattern

(* [same_list x y] can check whether 0+ is the sublist of both side of
   stm.curr *)
let rec same_list x y =
  match (x, y) with
  | [], _ -> true
  | _, [] -> false
  | h :: t, h' :: t' -> h = h' && same_list t t'

(* [sublist x y acc] returns whether stm contains +0 or not, acc which is the
   equivalent of y in y+0, and the part of stm after +0 *)
let rec sublist x y acc =
  match (x, y) with
  | [], _ -> (true, acc, List.tl y)
  | _, [] -> (false, acc, y)
  | h :: _, h' :: t' ->
      if h = h' && same_list x y then (true, List.rev acc, List.tl t')
      else sublist x t' (h' :: acc)

let add_zero stm =
  let zero_exp = [ Num "0"; Opr '+' ] in
  match stm.curr with
  | [], [] -> stm (* not really necessary match with after check non-empty?*)
  | a, b -> (
      match sublist zero_exp a [] with
      | true, zero_equiv, rest -> { stm with curr = (zero_equiv @ rest, b) }
      | false, _, _ -> (
          match sublist zero_exp b [] with
          | true, zero_equiv, rest -> { stm with curr = (a, zero_equiv @ rest) }
          | false, _, _ -> raise NotZeroAddPattern))

let mul_zero stm =
  let zero_exp = [ Num "0"; Opr '*' ] in
  match stm.curr with
  | [], [] -> stm (* not really necessary match with after check non-empty?*)
  | a, b -> (
      match sublist zero_exp a [] with
      | true, zero_equiv, [] -> { stm with curr = ([ Num "0" ], b) }
      | true, zero_equiv, rest ->
          { stm with curr = (Num "0" :: Opr '+' :: rest, b) }
      | false, _, _ -> (
          match sublist zero_exp b [] with
          | true, zero_equiv, [] -> { stm with curr = (a, [ Num "0" ]) }
          | true, zero_equiv, rest -> { stm with curr = (a, Num "0" :: rest) }
          | false, _, _ -> raise NotZeroMulPattern))

let next_statement stm tech =
  match tech with
  | Refl ->
      if compare_exp (fst stm.curr) (snd stm.curr) then raise QED
      else raise NotReflexive
  | Rw str -> (
      match str with
      | "add_zero" -> add_zero stm
      | "mul_zero" -> mul_zero stm
      | s -> substitute stm (str |> exp_of_string))
