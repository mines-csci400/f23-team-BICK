open Javascript_ast;;
open Javascript_main;;
open Testing;;

let eval_prog p =
  Printf.printf "result: %s\n"
    (str_value (Lab6.eval p))
;;

let run_tests () =
  print_string "Running Lab 6 Tests\n";
  print_string "===================\n";
  print_tests Lab6.simple_to_num_tests;
  print_tests Lab6.simple_to_bool_tests;
  print_tests Lab6.simple_to_str_tests;
  print_tests Lab6.simple_expr_eval_tests;
  print_tests Lab6.simple_print_eval_tests;
  print_tests Lab6.simple_cond_eval_tests;
  print_tests Lab6.simple_str_eval_tests;
  print_tests Lab6.eval_tests;
  print_tests Lab6.cond_eval_tests;
  print_tests Lab6.str_eval_tests;
;;

(* Main *)
prog_driver "lab6" eval_prog run_tests
