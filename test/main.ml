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

(* let test_is_valid (name : string) stm (expected_output : bool) : test = name
   >:: fun _ -> assert_equal expected_output (is_valid stm) *)

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

let test_add_zero (name : string) stm (expected_output : stm) : test =
  name >:: fun _ ->
  assert_equal expected_output (add_zero stm) ~printer:string_of_stm

let test_add_zero_exception (name : string) stm : test =
  name >:: fun _ -> assert_raises NotAddZeroPattern (fun () -> add_zero stm)

let test_zero_add (name : string) stm (expected_output : stm) : test =
  name >:: fun _ ->
  assert_equal expected_output (zero_add stm) ~printer:string_of_stm

let test_zero_add_exception (name : string) stm : test =
  name >:: fun _ -> assert_raises NotZeroAddPattern (fun () -> zero_add stm)

let test_mul_zero (name : string) stm (expected_output : stm) : test =
  name >:: fun _ ->
  assert_equal expected_output (mul_zero stm) ~printer:string_of_stm

let test_mul_zero_exception (name : string) stm : test =
  name >:: fun _ -> assert_raises NotMulZeroPattern (fun () -> mul_zero stm)

let test_zero_mul (name : string) stm (expected_output : stm) : test =
  name >:: fun _ ->
  assert_equal expected_output (zero_mul stm) ~printer:string_of_stm

(* test for helper function find_zero, find_add*)

let print_test_tuple = function
  | true, b ->
      let str = "true" in
      str ^ " " ^ string_of_exp b
  | false, b ->
      let str = "false" in
      str ^ " " ^ string_of_exp b

let print_test_tuple_2 = function
  | true, a, b ->
      let str = "true" in
      str ^ "\n   " ^ string_of_exp a
  | false, a, b ->
      let str = "false" in
      str ^ " " ^ string_of_exp a

let rec opr_counter count_num count_opr = function
  | [] -> (count_num, count_opr)
  | Num _ :: t -> opr_counter (count_num + 1) count_opr t
  | Opr _ :: t -> opr_counter count_num (count_opr + 1) t

let counter_condition = function
  | count_num, count_opr -> count_num = count_opr + 1

let rec find_mul (x : Expression.t) (acc : Expression.t) (before : Expression.t)
    =
  match x with
  | [] -> (false, acc, before)
  | h :: t ->
      if h = Opr '*' && opr_counter 0 0 (List.rev acc) |> counter_condition then
        (true, t, before)
      else find_mul t (h :: acc) before

let test_find_mul (name : string) (stm : Expression.t) (acc : Expression.t)
    (before : Expression.t)
    (expected_output : bool * Expression.t * Expression.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (find_mul stm acc before)
    ~printer:print_test_tuple_2

(* let test_find_zero (name : string) lst1 lst2 (expected_output : bool *
   Expression.t) : test = name >:: fun _ -> assert_equal expected_output
   (find_zero lst1 lst2) ~printer:print_test_tuple *)

(* let test_find_add (name : string) lst1 lst2 lst3 (expected_output : bool *
   Expression.t * Expression.t) : test = name >:: fun _ -> assert_equal
   expected_output (find_add lst1 lst2 lst3) ~printer:print_test_tuple_2 *)

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

(* 3 = 3+0 *)
let curr_l_add_zero = [ Num "3" ]
let curr_r_add_zero = [ Num "3"; Num "0"; Opr '+' ]
let stm_add_zero = make_stm (curr_l_add_zero, curr_r_add_zero) equiv_1
let stm_add_zero_1_1 = make_stm (curr_l_add_zero, curr_l_add_zero) equiv_1

(* 3 = 0+3 *)
let curr_r_add_zero_3 = [ Num "0"; Num "3"; Opr '+' ]

(* 3 = 3*0 + 0 => 3*0 *)
let curr_l_add_zero_4 = [ Num "3"; Num "0"; Opr '*'; Num "0"; Opr '+' ]
let curr_r_add_zero_4 = [ Num "3"; Num "0"; Opr '*' ]

(* 7 = 4 + 3 + 0 => 7=4+3 *)
let curr_l_add_zero_5 = [ Num "4"; Num "3"; Opr '+'; Num "0"; Opr '+' ]
let curr_r_add_zero_5 = [ Num "4"; Num "3"; Opr '+' ]
let stm_add_zero_3 = make_stm ([ Num "7" ], curr_l_add_zero_5) equiv_1
let stm_add_zero_3_1 = make_stm ([ Num "7" ], curr_r_add_zero_5) equiv_1

(* 0 = 3*0 *)
let curr_r_mul_zero_2_1 = [ Num "3"; Num "0"; Opr '*' ]
let stm_mul_zero_2_1 = make_stm ([ Num "0" ], curr_r_mul_zero_2_1) equiv_1

(* 1 = 3*0+1 *)
let curr_r_add_zero_2 = [ Num "3"; Num "0"; Opr '*'; Num "1"; Opr '+' ]
let stm_add_zero_2 = make_stm ([ Num "1" ], curr_r_add_zero_2) equiv_1

(* 1 = 1*1+0*1+4*0 *)
let curr_r_mul_zero_3 =
  [
    Num "1";
    Num "1";
    Opr '*';
    Num "0";
    Num "1";
    Opr '*';
    Opr '+';
    Num "4";
    Num "0";
    Opr '*';
    Opr '+';
  ]

let stm_mul_zero_3 = make_stm ([ Num "1" ], curr_r_mul_zero_3) equiv_1

let stm_add_zero_2_2 =
  make_stm ([ Num "1" ], [ Num "0"; Num "1"; Opr '+' ]) equiv_1

let statement_test =
  [
    test_string_of_stm "test_string_of_stm 3y=3*(x+3)" stm_1
      "(3*y) = (3*(x+3)); ";
    (* notice equiv will output a space after the string & also a ;*)
    test_string_of_equiv "test_string_of_equiv y = x+3 " stm_1 "y = (x+3); ";
    (* test_is_valid "test_is_valid 3y=3*(x+3) -> false " stm_1 false;
       test_is_valid "test_is_valid 2*4 = 2*4 -> false " stm_2 true; *)
    test_substitute "test substitute y=x+3 -> 3y = 3(x+3) " stm_1_1 e_1 stm_1_2;
    test_substitute_exception
      "test substitute exception y=x+3 -> 3y replace y+1 -> notmatch " stm_1_1
      e_2;
    test_next_statement "test next statement y=x+3 rw y" stm_1 (parse "rw y")
      stm_1_2;
    test_next_statement "test next statement 3=3+0 rw add_zero" stm_add_zero
      (parse "rw add_zero") stm_add_zero_1_1;
    test_next_statement_exception
      "test next statement exception 3y=3(x+3) refl -> not refl"
      (make_stm (curr_left_1, curr_right_1) equiv_1)
      (parse "refl") "not_refl";
    test_next_statement_exception
      "test next statement exception 3y=3y refl -> qed"
      (make_stm (curr_left_1, curr_left_1) equiv_1)
      (parse "refl") "qed";
    test_mul_zero "test mul zero 1=3*0+1 -> 1=0+1" stm_add_zero_2
      stm_add_zero_2_2;
    test_mul_zero "test mul zero 0=3*0 -> 0=0" stm_mul_zero_2_1
      (make_stm ([ Num "0" ], [ Num "0" ]) equiv_1);
    (* malform！！！！ *)
    (* test_mul_zero "test mul zero 1 = 1*1+0*1+4*0 -> 1=1+0*1+0" stm_mul_zero_3
       (make_stm ( [ Num "1" ], [ Num "1"; Num "0"; Num "1"; Opr '*'; Opr '+';
       Num "0"; Opr '+' ] ) equiv_1); *)
    (* malform above！！！！ *)
    test_add_zero "test add zero 7=4+3+0 -> 7=4+3" stm_add_zero_3
      stm_add_zero_3_1;
    test_add_zero "test add zero 3=3+0 -> 3=3" stm_add_zero stm_add_zero_1_1;
    test_add_zero "test add zero 3+0=3+0 -> 3=3+0"
      (make_stm (curr_r_add_zero, curr_r_add_zero) equiv_1)
      stm_add_zero;
    test_add_zero "test add zero 3=3*0+0 -> 3=3*0"
      (make_stm (curr_l_add_zero, curr_l_add_zero_4) equiv_1)
      (make_stm (curr_l_add_zero, curr_r_add_zero_4) equiv_1);
    test_add_zero_exception "test add zero 3y=3*(x+3) -> not add_zero_pattern"
      stm_1;
    test_add_zero_exception "test add zero 3 = 3*0+1 -> not add_zero_pattern"
      stm_add_zero_2;
    test_add_zero_exception "test add zero 3 = 0+3 -> not add_zero_pattern"
      (make_stm (curr_l_add_zero, curr_r_add_zero_3) equiv_1);
    (* test for zero add*)
    (* test_find_add "test find add +3 -> true" [ Num "3"; Opr '+' ] [] []
       (true, [ Num "3" ], []); test_find_zero "test find zero 0+3 -> true"
       curr_r_add_zero_3 [] (true, [ Num "3" ]); *)
    test_zero_add "test zero add\n       0+3=3 -> 3 =3"
      (make_stm (curr_r_add_zero_3, curr_l_add_zero) equiv_1)
      stm_add_zero_1_1;
    test_zero_add "test zero add\n 3=0+3 -> 3 =3"
      (make_stm (curr_l_add_zero, curr_r_add_zero_3) equiv_1)
      stm_add_zero_1_1;
    (* test_find_zero "test find_zero 7, 4+0+3 -> true" [ Num "4"; Num "0"; Opr
       '+'; Num "3"; Opr '+' ] [] (true, [ Num "4"; Num "3"; Opr '+' ]); *)
    test_zero_add "test zero add 7=4+0+3 -> 7 =7"
      (make_stm
         ([ Num "7" ], [ Num "4"; Num "0"; Opr '+'; Num "3"; Opr '+' ])
         equiv_1)
      (make_stm ([ Num "7" ], [ Num "4"; Num "3"; Opr '+' ]) equiv_1);
    (* test_find_add "test find\n add 3*5 [] [1] -> (true,3*5,1)" [ Num "3"; Num
       "5"; Opr '*'; Opr '+' ] [] [ Num "1" ] (true, [ Num "3"; Num "5"; Opr '*'
       ], [ Num "1" ]); test_find_zero "test find zero 16 1+0+3*5 -> true" [ Num
       "1"; Num "0"; Opr '+'; Num "3"; Num "5"; Opr '*'; Opr '+' ] [] (true, [
       Num "1"; Num "3"; Num "5"; Opr '*'; Opr '+' ]); *)
    test_zero_add "test zero add\n       16=1+0+3*5 -> 16 =1+3*5"
      (make_stm
         ( [ Num "16" ],
           [ Num "1"; Num "0"; Opr '+'; Num "3"; Num "5"; Opr '*'; Opr '+' ] )
         equiv_1)
      (make_stm
         ([ Num "16" ], [ Num "1"; Num "3"; Num "5"; Opr '*'; Opr '+' ])
         equiv_1);
    test_zero_add "test zero add 16=3*5+0+1 -> 16 =3*5+1"
      (make_stm
         ( [ Num "16" ],
           [ Num "3"; Num "5"; Opr '*'; Num "0"; Opr '+'; Num "1"; Opr '+' ] )
         equiv_1)
      (make_stm
         ([ Num "16" ], [ Num "3"; Num "5"; Opr '*'; Num "1"; Opr '+' ])
         equiv_1);
    test_zero_mul "test zero mul 0*1=0 0=0"
      (make_stm ([ Num "0"; Num "1"; Opr '*' ], [ Num "0" ]) equiv_1)
      (make_stm ([ Num "0" ], [ Num "0" ]) equiv_1);
    test_zero_mul "test zero mul 0*1+0=0 0=0"
      (make_stm
         ([ Num "0"; Num "1"; Opr '*'; Num "0"; Opr '+' ], [ Num "0" ])
         equiv_1)
      (make_stm ([ Num "0"; Num "0"; Opr '+' ], [ Num "0" ]) equiv_1);
    test_zero_mul "test zero mul 0*1+1=1 0+1=1"
      (make_stm
         ([ Num "0"; Num "1"; Opr '*'; Num "1"; Opr '+' ], [ Num "1" ])
         equiv_1)
      (make_stm ([ Num "0"; Num "1"; Opr '+' ], [ Num "1" ]) equiv_1);
    test_zero_mul "test zero mul 1+0*1+1=2 1+0+1=2"
      (make_stm
         ( [ Num "1"; Num "0"; Num "1"; Opr '*'; Opr '+'; Num "1"; Opr '+' ],
           [ Num "2" ] )
         equiv_1)
      (make_stm
         ([ Num "1"; Num "0"; Opr '+'; Num "1"; Opr '+' ], [ Num "2" ])
         equiv_1);
    test_zero_mul "test zero mul 3*5+0*1=16 -> 3*5+0=16"
      (make_stm
         ( [ Num "3"; Num "5"; Opr '*'; Num "0"; Num "1"; Opr '*'; Opr '+' ],
           [ Num "16" ] )
         equiv_1)
      (make_stm
         ([ Num "3"; Num "5"; Opr '*'; Num "0"; Opr '+' ], [ Num "16" ])
         equiv_1);
    test_zero_mul "test zero mul 16=3*5+0*1 -> 16 =3*5+0"
      (make_stm
         ( [ Num "16" ],
           [ Num "3"; Num "5"; Opr '*'; Num "0"; Num "1"; Opr '*'; Opr '+' ] )
         equiv_1)
      (make_stm
         ([ Num "16" ], [ Num "3"; Num "5"; Opr '*'; Num "0"; Opr '+' ])
         equiv_1);
    test_zero_mul "test zero mul 16=3*5+0*1*3 -> 16 =3*5+0*3"
      (make_stm
         ( [ Num "16" ],
           [
             Num "3";
             Num "5";
             Opr '*';
             Num "0";
             Num "1";
             Opr '*';
             Num "3";
             Opr '*';
             Opr '+';
           ] )
         equiv_1)
      (make_stm
         ( [ Num "16" ],
           [ Num "3"; Num "5"; Opr '*'; Num "0"; Num "3"; Opr '*'; Opr '+' ] )
         equiv_1);
  ]

let test_parse (name : string) str (expected_output : technique) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let technique_test = []

let suite =
  "test suite for Prover"
  >::: List.flatten [ expression_tests; statement_test; technique_test ]

let _ = run_test_tt_main suite