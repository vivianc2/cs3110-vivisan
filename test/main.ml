open OUnit2
open Prover
open Expression
open Statement
open Technique

(* quote: https://gist.github.com/philtomson/960227*)
let string_rev str =
  let rec aux idx =
    match idx with
    | 0 -> Char.escaped str.[0]
    | _ -> Char.escaped str.[idx] ^ aux (idx - 1)
  in
  aux (String.length str - 1)

let print_element_list e =
  let s =
    List.fold_left
      (fun acc x ->
        match x with
        | Opr c -> String.make 1 c ^ acc
        | Num n -> n ^ acc)
      "" e
  in
  string_rev s

let print_string s : string = s
let print_bool b = string_of_bool b

(** [test_infix_of_string name str expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [infix_of_string str]. *)
let test_infix_of_string name str (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (infix_of_string str) ~printer:print_element_list

let test_exp_of_infix name inf (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (exp_of_infix inf) ~printer:print_element_list

let test_string_of_exp name post (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_exp post) ~printer:print_string

let test_exp_of_string name str (expected_output : element list) : test =
  name >:: fun _ ->
  assert_equal expected_output (exp_of_string str) ~printer:print_element_list

let test_compare_op name e1 e2 (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (compare_op e1 e2) ~printer:print_bool

let test_compare_exp name e1 e2 (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (compare_exp e1 e2) ~printer:print_bool

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

let t5 =
  [
    Num "0";
    Opr '/';
    Opr '(';
    Opr '(';
    Num "2";
    Opr '+';
    Num "3";
    Opr ')';
    Opr '*';
    Num "4";
    Opr '+';
    Num "1";
  ]

let t7 =
  [
    Opr '(';
    Num "0";
    Opr '/';
    Opr '(';
    Opr '(';
    Opr '(';
    Num "2";
    Opr '+';
    Num "3";
    Opr ')';
    Opr '*';
    Num "4";
    Opr ')';
    Opr '+';
    Num "1";
    Opr ')';
    Opr ')';
  ]

let t6 =
  [
    Num "0";
    Num "2";
    Num "3";
    Opr '+';
    Num "4";
    Opr '*';
    Num "1";
    Opr '+';
    Opr '/';
  ]

let t8 = [ Num "2"; Opr '$'; Num "3"; Opr '+' ]
let t8_1 = [ Num "2"; Num "3"; Opr '+'; Opr '$' ]
let t9 = [ Opr '$'; Num "2"; Opr '+'; Num "3" ]
let t9_1 = [ Opr '$'; Opr '('; Num "2"; Opr '+'; Num "3"; Opr ')' ]
let t10 = [ Num "2"; Opr '$'; Num "3"; Opr '*'; Num "3"; Opr '$'; Opr '+' ]

let expression_tests =
  [
    test_infix_of_string "test_infix_of_string -> 2*(1+3)" "2*(1+3)" t1;
    test_infix_of_string "test_infix_of_string -> (1+(2+3)+(4))" "(1+(2+3)+(4))"
      t3;
    test_infix_of_string "test_infix_of_string -> (0/(((2+3)*4)+1))"
      "(0/(((2+3)*4)+1))" t7;
    test_infix_of_string "test_infix_of_string -> $2+3" "$2+3" t9;
    test_infix_of_string "test_infix_of_string -> $(2+3)" "$(2+3)" t9_1;
    (* error *)
    (* test_infix_of_string "test_infix_of_string empty -> []" "" []; *)
    test_exp_of_infix "test_exp_of_infix 2*(1+3)" t1 t2;
    test_exp_of_infix "test_exp_of_infix (1+(2+3)+(4))" t3 t4;
    (* error fixed *)
    test_exp_of_infix "test_exp_of_infix 0/((2+3)*4+1)" t5 t6;
    test_exp_of_infix "test_exp_of_infix $ 2+3" t9 t8;
    test_exp_of_infix "test_exp_of_infix $(2+3)" t9_1 t8_1;
    test_string_of_exp "test_string_of_exp t6 -> (0/(((2+3)*4)+1))" t6
      "(0/(((2+3)*4)+1))";
    test_string_of_exp "test_string_of_exp t2 -> 2*(1+3)" t2 "(2*(1+3))";
    test_string_of_exp "test_string_of_exp t4 -> (1+(2+3)+(4))" t4
      "((1+(2+3))+4)";
    test_string_of_exp "test_string_of_exp t8 -> $2+3" t8 "(($2)+3)";
    test_string_of_exp "test_string_of_exp t8_1 -> ($(2+3))" t8_1 "($(2+3))";
    test_exp_of_string "test_string_of_exp 0/((2+3)*4+1)" "0/((2+3)*4+1)" t6;
    test_exp_of_string "test_string_of_exp (0/(((2+3)*4)+1))"
      "(0/(((2+3)*4)+1))" t6;
    test_exp_of_string "test_exp_of_string $2+3" "$2+3" t8;
    test_exp_of_string "test_exp_of_string $2*3+$3" "$2*3+$3" t10;
    test_exp_of_string "test_string_of_exp (2*(1+3))" "(2*(1+3))" t2;
    test_exp_of_string "test_string_of_exp 2*(1+3)" "2*(1+3)" t2;
    test_exp_of_string "test_string_of_exp (1+(2+3)+(4))" "(1+(2+3)+(4))" t4;
    test_compare_op "compare op * + -> false" '*' '+' false;
    test_compare_op "compare op * * -> false" '*' '*' false;
    test_compare_op "compare op * / -> false" '*' '/' false;
    test_compare_op "compare op * ( -> false" '*' '(' false;
    test_compare_op "compare op ( + -> true" '(' '+' true;
    test_compare_exp "compare exp [] [] -> true" [] [] true;
    test_compare_exp "compare exp t4 (+t4 -> false" t4 (Opr '(' :: t4) false;
    test_compare_exp "compare exp t4 t3 -> false" t4 t3 false;
    test_compare_exp "compare exp t4 t4 -> true" t4 t4 true;
  ]

(* let print_str x = x *)

let test_string_of_stm (name : string) stm (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_stm stm) ~printer:print_string

let test_string_of_equiv (name : string) stm (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_equiv stm) ~printer:print_string

let test_is_empty (name : string) stm (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (is_empty stm) ~printer:print_bool

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

let test_add_succ (name : string) stm (expected_output : stm) : test =
  name >:: fun _ ->
  assert_equal expected_output (add_succ stm) ~printer:string_of_stm

let test_add_succ_exception (name : string) stm : test =
  name >:: fun _ -> assert_raises NotSuccPattern (fun () -> add_succ stm)

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
let curr_left_111 = [ Num "3"; Num "y"; Opr '*'; Num "y"; Opr '+' ]

let curr_left_112 =
  [ Num "3"; Num "x"; Num "3"; Opr '+'; Opr '*'; Num "y"; Opr '+' ]

let stm_1_1_1 = make_stm (curr_left_111, curr_left_1) equiv_1
let stm_1_2_1 = make_stm (curr_left_112, curr_right_1) equiv_1

let curr_left_113 =
  [
    Num "3";
    Num "x";
    Num "3";
    Opr '+';
    Opr '*';
    Num "x";
    Num "3";
    Opr '+';
    Opr '+';
  ]

let stm_1_1_2 = make_stm (curr_left_112, curr_right_1) equiv_1
let stm_1_2_2 = make_stm (curr_left_113, curr_right_1) equiv_1
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

let stm_zero_add = make_stm (curr_l_add_zero, curr_r_add_zero_3) equiv_1
let curr_zero_mul = [ Num "0"; Num "3"; Opr '*' ]
let stm_zero_mul = make_stm ([ Num "0" ], curr_zero_mul) equiv_1
let zero_zero = make_stm ([ Num "0" ], [ Num "0" ]) equiv_1

(* let stm_mul_zero_2_1 *)
let statement_tests =
  [
    test_string_of_stm "test_string_of_stm 3y=3*(x+3)" stm_1
      "(3*y) = (3*(x+3)); ";
    (* notice equiv will output a space after the string & also a ;*)
    test_string_of_equiv "test_string_of_equiv y = x+3 " stm_1 "y = (x+3); ";
    (* error fixed*)
    test_is_empty "test_is_empty empty -> true" (make_stm ([], []) []) true;
    test_is_empty "test_is_empty stm_1_1 -> false" stm_1_1 false;
    test_substitute "test substitute y=x+3 -> 3y = 3(x+3) " stm_1_1 e_1 stm_1_2;
    test_substitute "test substitute y=x+3 -> 3y+y = 3(x+3)+y " stm_1_1_1 e_1
      stm_1_2_1;
    test_substitute "test substitute y=x+3 -> 3(x+3)+y = 3(x+3)+(x+3) "
      stm_1_1_2 e_1 stm_1_2_2;
    test_substitute_exception
      "test substitute exception y=x+3 -> 3y replace y+1 -> notmatch " stm_1_1
      e_2;
    test_substitute_exception
      "test substitute exception y=x+3 -> 3y replace x -> notmatch " stm_1_1
      [ Num "x" ];
    test_substitute_exception
      "test substitute exception y=x+3 -> 3y replace 3 -> notmatch " stm_1_1
      [ Num "3" ];
    test_next_statement "test next statement y=x+3 rw y" stm_1 (parse "rw y")
      stm_1_2;
    test_next_statement "test next statement 3=3+0 rw add_zero" stm_add_zero
      (parse "rw add_zero") stm_add_zero_1_1;
    test_next_statement "test next statement 3=3*0 rw mul_zero" stm_mul_zero_2_1
      (parse "rw mul_zero") zero_zero;
    test_next_statement "test next statement 3=0+3 rw zero_add" stm_zero_add
      (parse "rw zero_add") stm_add_zero_1_1;
    test_next_statement "test next statement 3=0*3 rw zero_mul" stm_zero_mul
      (parse "rw zero_mul") zero_zero;
    test_next_statement_exception
      "test next statement exception 3y=3(x+3) refl -> not refl"
      (make_stm (curr_left_1, curr_right_1) equiv_1)
      (parse "refl") "not_refl";
    test_next_statement_exception
      "test next statement exception 3y=3(x+3) rw mul_zero -> NotMulZeroPattern"
      (make_stm (curr_left_1, curr_right_1) equiv_1)
      (parse "rw mul_zero") "NotMulZeroPattern";
    test_next_statement_exception
      "test next statement exception 3=3*0 rw zero_mul -> NotZeroMulPattern"
      stm_mul_zero_2_1 (parse "rw zero_mul")
      "NotMulZeroPattNotZeroMulPatternern";
    test_next_statement_exception
      "test next statement exception 3y=3y refl -> qed"
      (make_stm (curr_left_1, curr_left_1) equiv_1)
      (parse "refl") "qed";
  ]

let curr_r_4_add_0_3 = [ Num "4"; Num "0"; Opr '+'; Num "3"; Opr '+' ]

let add_mul_zero_tests =
  [
    test_mul_zero "test mul zero 1=3*0+1 -> 1=0+1" stm_add_zero_2
      stm_add_zero_2_2;
    test_mul_zero "test mul zero 0=3*0 -> 0=0" stm_mul_zero_2_1
      (make_stm ([ Num "0" ], [ Num "0" ]) equiv_1);
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
      stm_zero_add;
    test_zero_add "test zero add 0+3=3 -> 3 =3"
      (make_stm (curr_r_add_zero_3, curr_l_add_zero) equiv_1)
      stm_add_zero_1_1;
    test_zero_add "test zero add\n 3=0+3 -> 3 =3" stm_zero_add stm_add_zero_1_1;
    test_zero_add "test zero add 7=4+0+3 -> 7 =7"
      (make_stm ([ Num "7" ], curr_r_4_add_0_3) equiv_1)
      (make_stm ([ Num "7" ], [ Num "4"; Num "3"; Opr '+' ]) equiv_1);
    test_zero_add "test zero add 16=1+0+3*5 -> 16 =1+3*5"
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
      zero_zero;
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

(* let t8 = [ Num "2"; Opr '$'; Num "3"; Opr '+' ] let t8_1 = [ Num "2"; Num
   "3"; Opr '+'; Opr '$' ] *)
let stm_succ_1 = make_stm (t8_1, t8) equiv_1
let stm_succ_11 = make_stm (t8_1, t8_1) equiv_1

(* let t10 = [ Num "2"; Opr '$'; Num "3"; Opr '*'; Num "3"; Opr '$'; Opr '+'
   ] *)
let t10_1 = [ Num "2"; Num "3"; Opr '*'; Opr '$'; Num "3"; Opr '$'; Opr '+' ]
let stm_succ_2 = make_stm (t10, t10_1) equiv_1
let stm_succ_21 = make_stm (t10_1, t10_1) equiv_1
let t11 = [ Num "2"; Num "3"; Opr '*'; Num "3"; Opr '$'; Opr '+' ]
let t11_1 = [ Num "2"; Num "3"; Opr '*'; Num "3"; Opr '+'; Opr '$' ]
let stm_succ_3 = make_stm (t11, t11_1) equiv_1
let stm_succ_31 = make_stm (t11_1, t11_1) equiv_1
let t12 = [ Num "2"; Num "3"; Opr '$'; Opr '*' ]
let stm_succ_4 = make_stm (t12, t12) equiv_1

(* $2*2 *)
let stm_not_succ =
  make_stm
    ( [ Num "16" ],
      [ Num "3"; Num "5"; Opr '*'; Num "0"; Num "1"; Opr '*'; Opr '+' ] )
    equiv_1

let succ_tests =
  [
    test_add_succ "test_add_succ $2+3 -> $(2+3)" stm_succ_1 stm_succ_11;
    test_add_succ "test_add_succ $2*3+$3 -> $(2*3)+$3" stm_succ_2 stm_succ_21;
    test_add_succ_exception "test_add_succ_exception 16=3*5+0*1" stm_not_succ;
    test_add_succ "test_add_succ 2*3+$3 -> $(2*3+3)" stm_succ_3 stm_succ_31;
    test_add_succ_exception "test_add_succ_exception 2*$3" stm_succ_4;
  ]

let test_parse (name : string) str (expected_output : technique) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let test_parse_exception (name : string) str h : test =
  name >:: fun _ -> assert_raises h (fun () -> parse str)

let malform_err = Prover.Technique.Malformed

let technique_tests =
  [
    test_parse "test parse rw x -> Rw x" "rw x" (Rw "x");
    test_parse "test parse rw refl -> rw refl" "rw refl" (Rw "refl");
    test_parse_exception "test parse refl x -> malform failure" "refl x"
      malform_err;
    test_parse "test parse refl -> Refl" "refl" Refl;
    test_parse_exception "test parse help -> raise ShowHelp " "help"
      Prover.Technique.ShowHelp;
    test_parse_exception "test parse help x -> malform " "help x" malform_err;
    test_parse_exception "test parse quit -> raise Quit" "quit"
      Prover.Technique.Quit;
    test_parse_exception "test parse quit x -> malform" "quit x" malform_err;
  ]

let suite =
  "test suite for Prover"
  >::: List.flatten
         [
           expression_tests;
           statement_tests;
           technique_tests;
           add_mul_zero_tests;
         ]

let _ = run_test_tt_main suite