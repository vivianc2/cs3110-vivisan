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

(** [test_make_stm name x y expected_output] *)
let test_make_stm (name : string) x y (expected_output : stm) : test =
  name >:: fun _ -> assert_equal expected_output (make_stm x y)

let test_string_of_equiv (name : string) stm (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (string_of_equiv stm)

let test_is_valid (name : string) stm (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_valid stm)

let test_substitute (name : string) stm e (expected_output : stm) : test =
  name >:: fun _ -> assert_equal expected_output (substitute stm e)

let test_next_statement (name : string) stm tech (expected_output : stm) : test
    =
  name >:: fun _ -> assert_equal expected_output (next_statement stm tech)

let statement_test = []

let test_parse (name : string) str (expected_output : technique) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let technique_test = []

let suite =
  "test suite for Prover"
  >::: List.flatten [ expression_tests; statement_test; technique_test ]

let _ = run_test_tt_main suite