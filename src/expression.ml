type element =
  | Num of string
  | Opr of char

type t = element list

exception Malformed

let infix_of_string str =
  let rec infix_of_string_helper str curr =
    if String.length str = 0 then
      if String.length curr > 0 then [ Num curr ] else []
    else
      match str.[0] with
      | '+' | '-' | '*' | '/' | '(' | ')' ->
          let tail =
            Opr str.[0]
            :: infix_of_string_helper
                 (String.sub str 1 (String.length str - 1))
                 ""
          in
          if String.length curr > 0 then Num curr :: tail else tail
      | c ->
          infix_of_string_helper
            (String.sub str 1 (String.length str - 1))
            (curr ^ String.make 1 c)
  in
  infix_of_string_helper str ""

(** [compare_op c1 c2] is true if and only if c1 is an operation of lower 
  priority than c2 *)
let compare_op c1 c2 =
  match (c1, c2) with
  | '(', _ -> true
  | ('+' | '-' ), ('*' | '/') -> true
  | _, _ -> false

let exp_of_infix inf =
  let rec exp_of_infix_helper exp inf sign_lst =
    let rec update_exp exp sign_lst c =
      match (c, sign_lst) with
      | ')', [] -> (exp, [])
      | ')', h :: t ->
          if h = '(' then (exp, t) else update_exp (exp @ [Opr h]) t c
      | _, [] -> (exp, [ c ])
      | _, h :: t ->
          if compare_op h c then (exp, c :: sign_lst)
          else update_exp (exp @ [Opr h]) t c
    in
    match inf with
    | [] -> begin
        match sign_lst with
        | [] -> exp
        | h :: t -> exp_of_infix_helper (exp @ [Opr h]) inf t
      end
    | h :: t -> begin
        match h with
        | Num n -> exp_of_infix_helper (exp @ [Num n]) t sign_lst
        | Opr c ->
            if c = '(' then exp_of_infix_helper exp t (c :: sign_lst)
            else
              let new_exp, new_sign_lst = update_exp exp sign_lst c in
              exp_of_infix_helper new_exp t new_sign_lst
      end
  in
  exp_of_infix_helper [] inf []

let string_of_exp post =
  let rec infix_of_postfix_helper post inf =
    match post with
    | [] -> inf
    | h :: t -> (
        match h with
        | Num n -> infix_of_postfix_helper t (n :: inf)
        | Opr c -> (
            match inf with
            | [] | [ _ ] -> raise Malformed
            | y :: z :: s ->
                infix_of_postfix_helper t
                  (("(" ^ z ^ String.make 1 c ^ y ^ ")") :: s)))
  in
  infix_of_postfix_helper post [] |> List.fold_left (fun x y -> x ^ y) ""

let rec compare_exp post1 post2 =
  match (post1, post2) with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | h1 :: t1, h2 :: t2 -> h1 = h2 || compare_exp t1 t2
