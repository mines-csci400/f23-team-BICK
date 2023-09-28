open Testing
open Lab3

let main () =
  print_string "Running Lab 3\n";
  print_string "=============\n";

  Testing.print_tests Lab3.map_inorder_tests;
  Testing.print_tests Lab3.map_revorder_tests;
  Testing.print_tests Lab3.is_bst_tests_int;
  Testing.print_tests Lab3.is_bst_tests_str;
  Testing.print_tests Lab3.bst_max_tests;
  Testing.print_tests Lab3.bst_min_tests;
  Testing.print_tests Lab3.bst_insert_tests_int;
  Testing.print_tests Lab3.bst_insert_tests_str;
  Testing.print_tests Lab3.bst_search_tests_int;
  Testing.print_tests Lab3.bst_search_tests_str;
  Testing.print_tests Lab3.bst_remove_min_tests;
  Testing.print_tests Lab3.bst_remove_tests_int;
  Testing.print_tests Lab3.bst_remove_tests_str;

;;

main ()
