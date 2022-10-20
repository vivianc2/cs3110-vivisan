open Prover
open Expression
open Technique
open Statement

(* These should probably be put into a seperate file *)
let count = ref 1

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

let stm3 =
  make_stm
    (exp_of_string "a*(b+c)+d", exp_of_string "w*x+(y+z)")
    [
      (exp_of_string "a", exp_of_string "w");
      (exp_of_string "b+c", exp_of_string "x");
      (exp_of_string "d", exp_of_string "y+z");
    ]

let stm4 =
  make_stm
    (exp_of_string "x+0", exp_of_string "x")
    [
      
    ]
    

let stm_lst = [ stm1; stm2; stm3 ; stm4]

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game stm =
  ANSITerminal.print_string
    [ ANSITerminal.cyan; ANSITerminal.Bold ]
    (string_of_stm stm);
  print_string "\n> ";
  match next_statement stm (parse (read_line ())) with
  | exception Malformed ->
      print_endline "Your command is malformed! Please try again.";
      play_game stm
  | exception NotMatch ->
      print_endline "The expression to rewrite is not found! Please try again.";
      play_game stm
  | exception ShowHelp ->
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "The equivalent expressions that you know are:";
      ANSITerminal.print_string
        [ ANSITerminal.green; ANSITerminal.Bold ]
        ("\t" ^ string_of_equiv stm ^ "\n");
      play_game stm
  | exception NotReflexive ->
      print_endline "The expression is not reflexive! Please try again.";
      play_game stm
  | exception QED ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\nQ.E.D\n\n"
  | exception Quit ->
      print_endline "You quit the prover. See you next time~";
      exit 0
  | new_stm -> play_game new_stm

let rec go_through_stm_lst stm_lst =
  match stm_lst with
  | [] ->
      print_endline "You've finished all the proofs. Great job!";
      exit 0
  | h :: t -> (
      ANSITerminal.print_string
        [ ANSITerminal.yellow; ANSITerminal.Underlined ]
        ("Level " ^ string_of_int !count ^ "\n");
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "The equivalent expressions that you know are:";
      ANSITerminal.print_string
        [ ANSITerminal.green; ANSITerminal.Bold ]
        ("\t" ^ string_of_equiv h ^ "\n");
      match play_game h with
      | exception Retry ->
          print_endline "";
          go_through_stm_lst stm_lst
      | _ ->
          incr count;
          go_through_stm_lst t)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\n\nWelcome to Mathematical Prover!\n\n";
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "Valid operations include thses techniques: ";
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "refl, rw [variable_name].\n";
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "Other commands you can use include: ";
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "help, quit, retry\n\n";
  go_through_stm_lst stm_lst

(* Execute the game engine. *)
let () = main ()