open Testing
open Lab4

let main () =
  print_string "Running Lab 4\n";
  print_string "=============\n";
  Testing.print_tests Lab4.rbt_is_invariant_int_tests;
  Testing.print_tests Lab4.rbt_is_invariant_str_tests;
  Testing.print_tests Lab4.rbt_is_sorted_int_tests;
  Testing.print_tests Lab4.rbt_is_sorted_str_tests;
  Testing.print_tests Lab4.rbt_search_int_tests;
  Testing.print_tests Lab4.rbt_search_str_tests;
  Testing.print_tests Lab4.rbt_balance_int_tests;
  Testing.print_tests Lab4.rbt_balance_str_tests;
  Testing.print_tests Lab4.rbt_insert_int_tests;
  Testing.print_tests Lab4.rbt_insert_str_tests;
;;

main ()
