type technique =
  | Refl
  | Rw of string
  | Ind of string list
  (** Ind has three elements: the varibale to induct on, the name of inductive variable 
  and the name of inductive hypothesis. Requires the string to have exactly five words:
  induction [var] with [d] [hd]*)

exception Empty
exception Malformed
exception ShowHelp
exception Quit
exception Retry

let parse str =
  let words =
    str |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
  in
  match words with
  | [] -> raise Malformed
  | h :: t ->
      if h = "rw" then
        if List.length t > 0 then Rw (List.fold_left ( ^ ) "" t)
        else raise Malformed
      else if h = "refl" then
        if List.length t > 0 then raise Malformed else Refl
      else if h = "induction" then
        if List.length t = 4 then Ind (List.filter (fun s -> s <> "with") t)
        else raise Malformed
      else if h = "help" then
        if List.length t > 0 then raise Malformed else raise ShowHelp
      else if h = "quit" then
        if List.length t > 0 then raise Malformed else raise Quit
      else if h = "retry" then
        if List.length t > 0 then raise Malformed else raise Retry
      else raise Malformed