type element =
    |Num of string
    |Opr of char

type t = element list

exception Malformed

(** TODO: convert infix to postfix exp*)
let string_of_exp post = 
  let rec infix_of_postfix_helper post inf = match post with
    | [] -> inf
    | h :: t -> match h with
      | Num n -> infix_of_postfix_helper t (n :: inf)
      | Opr c -> match inf with
        | [] | _::[] -> raise Malformed
        | y :: z :: s -> infix_of_postfix_helper t (("(" ^ z ^ String.make 1 c ^ y ^ ")") :: s)
  in
  infix_of_postfix_helper post [] |> List.fold_left (fun x y -> x ^ y) "" 

let rec compare_exp post1 post2 = match post1, post2 with
|[],[] -> true
|_,[] -> false
|[],_ -> false
|h1::t1, h2::t2 -> (h1 = h2) || (compare_exp t1 t2)

