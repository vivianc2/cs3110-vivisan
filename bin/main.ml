open Prover
open Expression
open Technique
open Statement

let count = ref 1
let data_dir_prefix = "data" ^ Filename.dir_sep
let member = Yojson.Basic.Util.member
let to_string = Yojson.Basic.Util.to_string
let to_list = Yojson.Basic.Util.to_list

let tuple_of_json j =
  ( j |> member "LHS" |> to_string |> exp_of_string,
    j |> member "RHS" |> to_string |> exp_of_string )

let stm_of_json j =
  make_stm
    (j |> member "equation" |> tuple_of_json)
    (try j |> member "equals" |> to_list |> List.map tuple_of_json
     with _ -> [])

let stm_lst_of_json j = j |> member "proofs" |> to_list |> List.map stm_of_json
let techniques_of_json j = j |> member "techniques" |> to_string

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game stm =
  ANSITerminal.print_string
    [ ANSITerminal.cyan; ANSITerminal.Bold ]
    (string_of_stm stm);
  print_string "\n> ";
  match next_statement stm (parse (read_line ())) with
  | exception Malformed ->
      print_endline "Your command is malformed!";
      print_endline "Please try again.";
      play_game stm
  | exception NotMatch ->
      print_endline
        "The expression to rewrite is not found or the function to rewrite has \
         the wrong name!";
      print_endline " Please try again.";
      play_game stm
  | exception ShowHelp ->
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "The equivalent expressions that you know are:";
      ANSITerminal.print_string
        [ ANSITerminal.green; ANSITerminal.Bold ]
        ("\t" ^ string_of_equiv stm ^ "\n");
      play_game stm
  | exception NotReflexive ->
      print_endline "The expression is not reflexive!";
      print_endline " Valid pattern: x=x, x+1 = x+1 and etc.";
      print_endline "Please try again.";
      play_game stm
  | exception QED ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\nQ.E.D\n\n"
  | exception NotAddZeroPattern ->
      print_endline "The expression is not an add zero pattern!";
      print_endline
        "Valid pattern is 3+0 or (3+1)+0. Do you mean rw zero_add? Please try \
         again.";
      play_game stm
  | exception NotMulZeroPattern ->
      print_endline "The expression is not a multiply zero pattern!";
      print_endline
        "Valid pattern is 3*0, (3+1)*0 and etc. Do you mean rw zero_mul? \
         Please try again.";
      play_game stm
  | exception NotZeroAddPattern ->
      print_endline "The expression is not a zero add pattern!";
      print_endline
        "Valid pattern is 0+3 or 0+(3+1). Do you mean rw add_zero? Please try \
         again.";
      play_game stm
  | exception NotZeroMulPattern ->
      print_endline "The expression is not a zero multiply pattern!";
      print_endline
        "Valid pattern is 0*3 or 0*(3+1). Do you mean rw zero_mul? Please try \
         again.";
      play_game stm
  | exception NotSuccPattern ->
      print_endline
        "The expression is not a succ addition pattern! Valid pattern is a + \
         succ b = succ (a+b).";
      print_endline "Please try again.";
      play_game stm
  | exception NotSuccEqPattern ->
      print_endline
        "The expression is not a succ Equal pattern! Valid pattern is a + succ \
         b = a (b+1).";
      print_endline "Please try again.";
      play_game stm
  | exception Quit ->
      print_endline "You have quit the prover. See you next time~";
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

let rec load_file f =
  match Yojson.Basic.from_file f with
  | json ->
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "Valid operations include thses techniques: ";
      let t = techniques_of_json json in
      ANSITerminal.print_string
        [ ANSITerminal.green; ANSITerminal.Bold ]
        (t ^ "\n");
      ANSITerminal.print_string [ ANSITerminal.Bold ]
        "Other commands you can use include: ";
      ANSITerminal.print_string
        [ ANSITerminal.green; ANSITerminal.Bold ]
        "help, quit, retry\n\n";
      go_through_stm_lst (stm_lst_of_json json)
  | exception e -> (
      print_endline
        "This input file doesn't exist. Please enter the correct file name \
         again.";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | file_name -> load_file (data_dir_prefix ^ file_name ^ ".json"))

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "\n\nWelcome to Mathematical Prover!\n\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_endline "Files available: 1.proofs \n";
  print_endline "If you want to quit the game, input quit";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> exit 0
  | file_name -> load_file (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()