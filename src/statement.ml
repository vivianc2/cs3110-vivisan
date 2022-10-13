open Expression

type stm = {
  curr : Expression.t * Expression.t;
  equiv : (Expression.t * Expression.t) list;
}

let make_stm x y = { curr = x; equiv = y }

let string_of_stm stm =
  (stm.curr |> fst |> string_of_exp) ^ "=" ^ (stm.curr |> snd |> string_of_exp)

let is_valid stm = compare_exp (stm.curr |> fst) (stm.curr |> snd)
let substitute a b = a
let next_statement a b = a