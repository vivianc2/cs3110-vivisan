open Prover
open Statement
open Expression
open Technique

(* These should probably be put into a seperate file *)
let t1 = exp_of_infix (infix_of_string "3*y")
let t2 = exp_of_infix (infix_of_string "3*(x+3)")
let t3 = exp_of_infix (infix_of_string "y")
let t4 = exp_of_infix (infix_of_string "x+3")
let stm1 = make_stm (t1, t2) [ (t3, t4) ]

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game stm =
  match next_statement stm (parse (read_line ())) with
  | new_stm ->
      print_endline (string_of_stm new_stm);
      print_string "> ";
      play_game new_stm
  | exception e ->
      print_endline "Great job!";
      Stdlib.exit 0

(* let input = raise (Failure "unfinished") let data_dir_prefix = "data" ^
   Filename.dir_sep *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to our prover!\n";
  (* print_endline "Please enter the name of the game file you want to
     load.\n"; *)
  print_endline (string_of_stm stm1);
  print_string "> ";
  play_game stm1
(* match read_line () with | exception End_of_file -> () | file_name ->
   play_game (data_dir_prefix ^ file_name ^ ".json") *)

(* Execute the game engine. *)
let () = main ()
