open Expression
open Technique

type stm = {
  curr : (t * t) list;
  equiv : (t * t) list;
}

let get_curr stm = stm.curr
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
let is_empty stm = stm.curr = [] || stm.curr = [ ([], []) ]

exception NotMatch

let rec substitute_chunk lst exp equiv_exp =
  match (lst, exp) with
  | a, [] -> equiv_exp @ a
  | [], _ -> raise NotMatch
  | h :: t, eh :: et ->
      if h = eh then substitute_chunk t et equiv_exp else raise NotMatch

(** [substitute_helper lst exp equiv_exp] is the lst with [exp] changed into
    [equiv_exp]. *)
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
    | [] -> stm
    | h :: t ->
        {
          stm with
          curr =
            ( substitute_helper (fst h) exp equiv_exp,
              substitute_helper (snd h) exp equiv_exp )
            :: t;
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
exception NotAddZeroPattern
exception NotMulZeroPattern
exception NotSuccPattern
exception NotSuccEqPattern

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
  | [] -> stm
  | (a, b) :: t -> (
      match sublist zero_exp a [] with
      | true, zero_equiv, rest ->
          { stm with curr = (zero_equiv @ rest, b) :: t }
      | false, _, _ -> (
          match sublist zero_exp b [] with
          | true, zero_equiv, rest ->
              { stm with curr = (a, zero_equiv @ rest) :: t }
          | false, _, _ -> raise NotAddZeroPattern))

let mul_zero stm =
  let zero_exp = [ Num "0"; Opr '*' ] in
  match stm.curr with
  | [] -> stm
  | (a, b) :: t -> (
      match sublist zero_exp a [] with
      | true, zero_equiv, [] -> { stm with curr = ([ Num "0" ], b) :: t }
      | true, zero_equiv, rest ->
          { stm with curr = (Num "0" :: Opr '+' :: rest, b) :: t }
      | false, _, _ -> (
          match sublist zero_exp b [] with
          | true, zero_equiv, [] -> { stm with curr = (a, [ Num "0" ]) :: t }
          | true, zero_equiv, rest ->
              { stm with curr = (a, Num "0" :: rest) :: t }
          | false, _, _ -> raise NotMulZeroPattern))

(** [opr_counter count_num count_opr] returns the number of number and operators
    in one expressions*)
let rec opr_counter count_num count_opr = function
  | [] -> (count_num, count_opr)
  | Num _ :: t -> opr_counter (count_num + 1) count_opr t
  | Opr '$' :: t -> opr_counter count_num count_opr t
  | Opr _ :: t -> opr_counter count_num (count_opr + 1) t

(** [counter_condition count_num count_opr] check whether the number of numbers
    is one more than number of operator in one expression*)
let counter_condition = function
  | count_num, count_opr -> count_num = count_opr + 1

let rec find_opr (x : Expression.t) (bop : element) (acc : Expression.t)
    (before : Expression.t) =
  match x with
  | [] -> (false, acc, before)
  | h :: t ->
      if h = bop && opr_counter 0 0 (List.rev acc) |> counter_condition then
        if bop = Opr '+' then (true, List.rev acc, before) else (true, t, before)
      else find_opr t bop (h :: acc) before

let rec find_zero x (bop : element) before_h =
  (* before_h represents the part before the '0'*)
  (* y represents the part before we find a match with 0*)
  match x with
  | [] -> (false, [])
  | h :: t ->
      if h = Num "0" then
        if before_h = [] then
          match find_opr t bop [] [] with
          | false, _, _ -> (false, [])
          | true, rest, before -> (true, rest)
        else
          (* match find_opr t bop [] (List.rev before_h) with *)
          match find_opr (List.tl t) bop [] (List.rev before_h) with
          | false, _, _ -> (false, [])
          | true, rest, before -> (true, before @ rest @ [ Opr '+' ])
      else
        let before = h :: before_h in
        find_zero t bop before

let zero_add stm =
  match stm.curr with
  | [] -> stm
  | (a, b) :: t -> (
      match find_zero a (Opr '+') [] with
      | true, e -> { stm with curr = (e, b) :: t }
      | false, _ -> (
          match find_zero b (Opr '+') [] with
          | true, e -> { stm with curr = (a, e) :: t }
          | false, _ -> raise NotZeroAddPattern))

let rec find_zero_mul x (bop : element) before_h =
  match x with
  | [] -> (false, [])
  | h :: t ->
      if h = Num "0" then
        if before_h = [] then
          match find_opr t bop [] [] with
          | false, _, _ -> (false, [])
          | true, rest, before -> (true, Num "0" :: rest)
        else
          match find_opr t bop [] (List.rev before_h) with
          | false, _, _ -> (false, [])
          | true, rest, before -> (true, before @ [ Num "0" ] @ rest)
      else
        let before = h :: before_h in
        find_zero_mul t bop before

let zero_mul stm =
  match stm.curr with
  | [] -> stm
  | (a, b) :: t -> (
      match find_zero_mul a (Opr '*') [] with
      | true, e -> { stm with curr = (e, b) :: t }
      | false, _ -> (
          match find_zero_mul b (Opr '*') [] with
          | true, e -> { stm with curr = (a, e) :: t }
          | false, _ -> raise NotZeroMulPattern))

(** add_succ (a b : mynat) : a + succ(b) = succ(a + b)*)
let add_succ_aux stm1 stm2 = stm1 @ stm2 @ [ Opr '$' ]

let rec split_e e e1 e2 =
  match e with
  | [] -> (e1, e2)
  | Num n :: t ->
      let e1 = Num n :: e1 in
      let e2 = List.tl e2 in
      if
        opr_counter 0 0 (List.rev e1) |> counter_condition
        && not (opr_counter 0 0 (List.rev e2) |> counter_condition)
      then (e1, e2)
      else split_e t e1 e2
  | Opr c :: t -> split_e t (Opr c :: e1) (List.tl e2)

let rec find_add_succ e before =
  match e with
  | [] -> raise NotSuccPattern
  | Opr '$' :: Opr '+' :: t ->
      let e1, e2 = split_e before [] before in
      (true, List.rev e1 @ e2 @ (Opr '+' :: Opr '$' :: t))
  | Num n :: t -> find_add_succ t (before @ [ Num n ])
  | Opr c :: t -> find_add_succ t (before @ [ Opr c ])

let succ_helper stm f =
  match stm.curr with
  | [] -> stm
  | (a, b) :: t ->
      let found, a = f a [] in
      if found then { stm with curr = (a, b) :: t }
      else
        let foundb, b = f b [] in
        if foundb then { stm with curr = (a, b) :: t } else stm

let add_succ stm = succ_helper stm find_add_succ

let rec find_succ_add e before =
  match e with
  | [] -> raise NotSuccPattern
  | Opr '$' :: t ->
      if before <> [] && List.(before |> rev |> hd) = Opr '*' then
        raise NotSuccPattern
      else
        let e1, e2 = split_e before [] before in
        (true, List.rev e1 @ e2 @ t @ [ Opr '$' ])
  | Num n :: t -> find_succ_add t (before @ [ Num n ])
  | Opr c :: t -> find_succ_add t (before @ [ Opr c ])

let succ_add stm = succ_helper stm find_succ_add

(** succ n = n+1 *)
let rec find_succ_n acc = function
  | [] -> acc
  | Opr c :: t ->
      let acc = Opr c :: acc in
      find_succ_n acc t
  | Num n :: t ->
      if opr_counter 0 0 (List.rev acc) |> counter_condition then acc
      else
        let acc = Num n :: acc in
        find_succ_n acc t

(* a + succ n = a + (n+1) a n $ + = a n 1 + + *)
(* a * succ n = a * (n+1) a n s * = a n 1 + * *)
(* succ n + a = (n+1) + a n s a + = n 1 + a + *)
(* succ n * a = (n+1) * a n s a * = n 1 + a * *)
let succ_addition_pattern e =
  match find_add_succ e [] with
  | e -> (true, false)
  | exception NotSuccPattern -> (
      match find_succ_add e [] with
      | e -> (false, true)
      | exception NotSuccPattern -> (false, false))

let rec succ_eq_check e before =
  match e with
  | [] -> raise NotSuccEqPattern
  | Opr '$' :: t -> begin
      match succ_addition_pattern e with
      | true, _ -> (true, before @ [ Num "1"; Opr '+' ] @ t)
      | false, true ->
          (true, before @ [ Num "1"; Opr '+' ] @ t (* raise NotSuccEqPattern *))
      | false, false -> raise NotSuccEqPattern
    end
  | Opr n :: t -> succ_eq_check t (before @ [ Opr n ])
  | Num n :: t -> succ_eq_check t (before @ [ Num n ])

let succ_eq stm = succ_helper stm succ_eq_check

let next_statement stm tech =
  match stm.curr with
  | [] -> raise QED
  | h :: t -> begin
      match tech with
      | Refl ->
          if compare_exp (fst h) (snd h) then
            if t = [] then raise QED else { stm with curr = t }
          else raise NotReflexive
      | Rw str -> begin
          match str with
          | "add_zero" -> add_zero stm
          | "mul_zero" -> mul_zero stm
          | "zero_add" -> zero_add stm
          | "zero_mul" -> zero_mul stm
          | "succ" -> add_succ stm
          | "succ_eq" -> succ_eq stm
          | s -> substitute stm (str |> exp_of_string)
        end
      | Ind strlst ->
          let var_exp = exp_of_string (List.nth strlst 0) in
          let d_exp = exp_of_string (List.nth strlst 1) in
          let d_succ_exp = exp_of_string (List.nth strlst 1 ^ "+1") in
          let zero = exp_of_string "0" in
          let base =
            ( substitute_helper (fst h) var_exp zero,
              substitute_helper (snd h) var_exp zero )
          in
          let hd =
            ( substitute_helper (fst h) var_exp d_exp,
              substitute_helper (snd h) var_exp d_exp )
          in
          let hd_succ =
            ( substitute_helper (fst h) var_exp d_succ_exp,
              substitute_helper (snd h) var_exp d_succ_exp )
          in
          { curr = base :: hd_succ :: t; equiv = hd :: stm.equiv }
    end
