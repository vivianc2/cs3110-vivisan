(* type expression = string list *)

type technique =
  | Refl
  | Rw of string

exception Empty
exception Malformed

let parse str =
  let words =
    str |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
  in
  match words with
  | [] -> raise Empty
  | h :: t ->
      if h = "rw" then
        if List.length t > 0 then Rw (List.fold_left ( ^ ) "" t)
        else raise Malformed
      else if h = "refl" then
        if List.length t > 0 then raise Malformed else Refl
      else raise Malformed