open Prover
open Expression
open Technique
open Statement

(* These should probably be put into a seperate file *)
let count = ref 0

let next () =
  count := !count + 1;
  !count

let stm1 =
  make_stm
    (exp_of_string "3*y", exp_of_string "3*(x+3)")
    [ (exp_of_string "y", exp_of_string "x+3") ]

let stm2 =
  make_stm
    (exp_of_string "y*z", exp_of_string "(x+2)*(x+3)")
    [
      (exp_of_string "y", exp_of_string "x+2");
      (exp_of_string "z", exp_of_string "x+3");
    ]

let stm_lst = [ stm1; stm2 ]

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game stm =
  if is_empty stm then print_endline "Q.E.D"
  else (print_endline (string_of_stm stm);
  print_string "> ";
  match next_statement stm (parse (read_line ())) with
  | exception Malformed ->
      print_endline "Your command is malformed! Please try again.";
      play_game stm
  | exception NotMatch ->
      print_endline "The expression to rewrite is not found! Please try again.";
      play_game stm
  | exception ShowHelp ->
      print_endline "The equivalent expressions that you know are:";
      print_endline ("\t" ^ string_of_equiv stm);
      play_game stm
  | exception NotReflexive ->
      print_endline "The expression is not reflexive! Please try again.";
      play_game stm
  | exception QED -> print_endline "Q.E.D"
  (* exit 0 *)
  | exception Quit ->
      print_endline "You quit the prover. See you next time~";
      exit 0
  | new_stm -> play_game new_stm)

let rec go_through_stm stm_lst =
  match stm_lst with
  | [] ->
      print_endline "You've finished all the proofs. Great job!";
      exit 0
  | h :: t -> (
      print_endline ("Level " ^ string_of_int (next ()));
      print_endline "The equivalent expressions that you know are:";
      print_endline ("\t" ^ string_of_equiv h);
      match play_game h with
      | exception Retry -> play_game h
      | _ -> go_through_stm t)
(* go_through_stm t *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to our prover!\n";
  print_endline
    "Valid operations include thses techniques: refl, rw [variable_name].\n";
  go_through_stm stm_lst

(* Execute the game engine. *)
let () = main ()