open OUnit2
open Util
open Ast

let t1 = Parser.parse_from_string "2"

let t2 = Parser.parse_from_string "2 + 3 * 5 mod 2"

let t3 = Parser.parse_from_string "fun x -> fun y -> y * (x + 3)"

let t4 = Parser.parse_from_string "f 2 3"

let t5 = Parser.parse_from_string "if (b && not c) && d then 1 else 2"

let t6 = Parser.parse_from_string "fun x -> y * (x + 3)"

    
let pretty_print_test1 test_ctxt =
  assert_equal true (equal t1 (Parser.parse_from_string (string_of_term t1)))

let pretty_print_test2 test_ctxt =
  assert_equal true (equal t2 (Parser.parse_from_string (string_of_term t2)))

let pretty_print_test3 test_ctxt =
  assert_equal true (equal t3 (Parser.parse_from_string (string_of_term t3)))

let pretty_print_test4 test_ctxt =
  assert_equal true (equal t4 (Parser.parse_from_string (string_of_term t4)))

let pretty_print_test5 test_ctxt =
  assert_equal true (equal t5 (Parser.parse_from_string (string_of_term t5)))

let find_free_var_test1 test_ctxt =
  assert_equal (find_free_var t1) None

let find_free_var_test2 test_ctxt =
  assert_equal (find_free_var t2) None

let find_free_var_test3 test_ctxt =
  assert_equal (find_free_var t3) None

let find_free_var_test4 test_ctxt =
  assert_equal (find_free_var t4) (Some ("f", { pos_line = 1; pos_col = 0 }))

let find_free_var_test5 test_ctxt =
  assert_equal (find_free_var t5) (Some ("b", { pos_line = 1; pos_col = 4 }))

let find_free_var_test6 test_ctxt =
  assert_equal (find_free_var t6) (Some ("y", { pos_line = 1; pos_col = 9 }))
    
let subst_test1 test_ctxt =
  assert_equal (subst t1 "x" t2) t1

let subst_test2 test_ctxt =
  assert_equal (subst t2 "y" t2) t2

let subst_test3 test_ctxt =
  assert_equal (subst t3 "x" t2) t3

let subst_test4 test_ctxt =
  assert_equal (subst t3 "y" t2) t3

let subst_test5 test_ctxt =
  assert_equal 
    true
    (equal (subst t4 "f" t3)
       (Parser.parse_from_string "(fun x -> fun y -> y * (x + 3)) 2 3"))

let subst_test6 test_ctxt =
  let t = Parser.parse_from_string "fun y -> x * x" in
  assert_equal 
    true
    (equal (subst t "x" t1)
       (Parser.parse_from_string "fun y -> 2 * 2"))
    
let suite =
  "Problem 2 suite" >:::
  ["pretty_print: test1" >:: pretty_print_test1;
   "pretty_print: test2" >:: pretty_print_test2;
   "pretty_print: test3" >:: pretty_print_test3;
   "pretty_print: test4" >:: pretty_print_test4;
   "pretty_print: test5" >:: pretty_print_test5;
   "find_free_var_test1" >:: find_free_var_test1;
   "find_free_var_test2" >:: find_free_var_test2;
   "find_free_var_test3" >:: find_free_var_test3;
   "find_free_var_test4" >:: find_free_var_test4;
   "find_free_var_test5" >:: find_free_var_test5;
   "find_free_var_test6" >:: find_free_var_test6;
   "subst_test1" >:: subst_test1; 
   "subst_test2" >:: subst_test2;
   "subst_test3" >:: subst_test3;
   "subst_test4" >:: subst_test4;
   "subst_test5" >:: subst_test5;
   "subst_test6" >:: subst_test6;
 ]

let () = run_test_tt_main suite

