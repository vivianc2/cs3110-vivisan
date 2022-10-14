open Prover
open Expression
open Technique
open Statement

(* These should probably be put into a seperate file *)
let t1 = exp_of_string "3*y"
let t2 = exp_of_string "3*(x+3)"
let t3 = exp_of_string "y"
let t4 = exp_of_string "x+3"
let stm1 = make_stm (t1, t2) [ (t3, t4) ]

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game stm =
  print_endline (string_of_stm stm);
  print_string "> ";
  match next_statement stm (parse (read_line ())) with
  | exception Malformed ->
      print_endline "Your command is malformed! Please try again.";
      play_game stm
  | exception NotMatch ->
      print_endline "The expression to rewrite is not found! Please try again.";
      play_game stm
  | exception ShowHelp ->
      print_endline "The equivalent expressions are:";
      print_endline ("\t" ^ string_of_equiv stm);
      play_game stm
  | exception NotReflexive ->
      print_endline "The expression is not reflexive! Please try again.";
      play_game stm
  | exception QED ->
      print_endline "Q.E.D";
      exit 0
  | new_stm -> play_game new_stm

(* let input = raise (Failure "unfinished") let data_dir_prefix = "data" ^
   Filename.dir_sep *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to our prover!\n";
  (* print_endline "Please enter the name of the game file you want to
     load.\n"; *)
  play_game stm1
(* match read_line () with | exception End_of_file -> () | file_name ->
   play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
