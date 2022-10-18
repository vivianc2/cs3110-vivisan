open OUnit2
open Prover
open Expression
open Statement
open Technique

(** [test_infix_of_string name str expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [infix_of_string str]. *)
let test_infix_of_string name str (expected_output : t) : test =
  name >:: fun _ -> assert_equal expected_output (infix_of_string str)

let test_exp_of_infix name inf (expected_output : t) : test =
  name >:: fun _ -> assert_equal expected_output (exp_of_infix inf)

let test_string_of_exp name post (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (string_of_exp post)

let t1 = [ Num "2"; Opr '*'; Opr '('; Num "1"; Opr '+'; Num "3"; Opr ')' ]
let t2 = [ Num "2"; Num "1"; Num "3"; Opr '+'; Opr '*' ]

let t3 =
  [
    Opr '(';
    Num "1";
    Opr '+';
    Opr '(';
    Num "2";
    Opr '+';
    Num "3";
    Opr ')';
    Opr '+';
    Opr '(';
    Num "4";
    Opr ')';
    Opr ')';
  ]

let t4 = [ Num "1"; Num "2"; Num "3"; Opr '+'; Opr '+'; Num "4"; Opr '+' ]

let expression_tests =
  [
    test_infix_of_string "2*(1+3)" "2*(1+3)" t1;
    test_infix_of_string "(1+(2+3)+(4))" "(1+(2+3)+(4))" t3;
    test_exp_of_infix "2*(1+3)" t1 t2;
    test_exp_of_infix "(1+(2+3)+(4))" t3 t4;
    test_string_of_exp "2*(1+3)" t2 "(2*(1+3))";
    test_string_of_exp "(1+(2+3)+(4))" t4 "((1+(2+3))+4)";
  ]

let print_str x = x

let test_string_of_stm (name : string) stm (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_stm stm) ~printer:print_str

let test_string_of_equiv (name : string) stm (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_equiv stm) ~printer:print_str

let test_is_valid (name : string) stm (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_valid stm)

let test_substitute (name : string) stm e (expected_output : stm) : test =
  name >:: fun _ ->
  assert_equal expected_output (substitute stm e) ~printer:string_of_stm

let test_substitute_exception (name : string) stm e : test =
  name >:: fun _ -> assert_raises NotMatch (fun () -> substitute stm e)

let test_next_statement (name : string) stm tech (expected_output : stm) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (next_statement stm tech) ~printer:string_of_stm

let test_next_statement_exception (name : string) stm tech str : test =
  name >:: fun _ ->
  match str with
  | "not_refl" -> assert_raises NotReflexive (fun () -> next_statement stm tech)
  | "qed" -> assert_raises QED (fun () -> next_statement stm tech)
  | _ -> ()

(* y=x+3 -> 3y = 3*(x+3) *)
let curr_left_1 = [ Num "3"; Num "y"; Opr '*' ]
let curr_right_1 = [ Num "3"; Num "x"; Num "3"; Opr '+'; Opr '*' ]
let equiv_1 = [ ([ Num "y" ], [ Num "x"; Num "3"; Opr '+' ]) ]
let stm_1 = make_stm (curr_left_1, curr_right_1) equiv_1
let stm_1_1 = make_stm (curr_left_1, curr_left_1) equiv_1
let stm_1_2 = make_stm (curr_right_1, curr_right_1) equiv_1
let e_1 = [ Num "y" ]
let e_2 = [ Num "y"; Num "1"; Opr '+' ]
let curr_left_2 = [ Num "4"; Num "2"; Opr '*' ]
let curr_right_2 = [ Num "4"; Num "2"; Opr '*' ]
let stm_2 = make_stm (curr_left_2, curr_right_2) equiv_1

let statement_test =
  [
    test_string_of_stm "test_string_of_stm 3y=3*(x+3)" stm_1 "(3*y) = (3*(x+3))";
    (* notice equiv will output a space after the string & also a ;*)
    test_string_of_equiv "test_string_of_equiv y = x+3 " stm_1 "y = (x+3); ";
    test_is_valid "test_is_valid 3y=3*(x+3) -> false " stm_1 false;
    test_is_valid "test_is_valid 2*4 = 2*4 -> false " stm_2 true;
    test_substitute "test substitute y=x+3 -> 3y = 3(x+3) " stm_1_1 e_1 stm_1_2;
    test_substitute_exception
      "test substitute exception y=x+3 -> 3y replace y+1 -> notmatch " stm_1_1
      e_2;
    test_next_statement "test next statement y=x+3 rw y" stm_1 (parse "rw y")
      stm_1_2;
    test_next_statement_exception
      "test next statement exception 3y=3(x+3) refl -> not refl"
      (make_stm (curr_left_1, curr_right_1) equiv_1)
      (parse "refl") "not_refl";
    test_next_statement_exception
      "test next statement exception 3y=3y refl -> qed"
      (make_stm (curr_left_1, curr_left_1) equiv_1)
      (parse "refl") "qed";
  ]

let test_parse (name : string) str (expected_output : technique) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let technique_test = []

let suite =
  "test suite for Prover"
  >::: List.flatten [ expression_tests; statement_test; technique_test ]

let _ = run_test_tt_main suite