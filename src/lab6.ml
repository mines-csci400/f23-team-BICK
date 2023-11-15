open Javascript_ast
open Javascript_main
open Util

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 *)

(* basic tests to show how the value conversion functions work (do not modify) *)
let simple_to_num_tests =
  ("Simple ToNum Conversions", to_num, (fun n1 n2 -> eq_float (n1,n2)), eq_exn, Some(str_value,str_float),
   [
     (None, NumVal(123.0), Ok(123.0));
     (None, BoolVal(true), Ok(1.0));
     (None, StrVal(""),    Ok(0.0));
  ])
let simple_to_bool_tests =
  ("Simple ToBool Conversions", to_bool, (=), eq_exn, Some(str_value,string_of_bool),
   [
     (None, BoolVal(true),  Ok(true));
     (None, NumVal(1.0),    Ok(true));
     (None, StrVal("true"), Ok(true));
  ])
let simple_to_str_tests =
  ("Simple ToStr Conversions", to_str, (=), eq_exn, Some(str_value,(fun x -> x)),
   [
     (None, StrVal("hello"), Ok("hello"));
     (None, BoolVal(true),   Ok("true"));
     (None, NumVal(1.234),   Ok("1.234"));
     (None, NumVal(1.000),   Ok("1"));
     (None, NumVal(0.00),    Ok("0"));
     (None, NumVal(100.01),  Ok("100.01"));
  ])

(* (eval p) should reduce a program to a *value* (if Node.js produces
 * a *value* for an example JavaScript program, your evaluator should
 * produce that same value).  *)

(* evaluate a program *)
let rec eval (p : program_t) : value_t = match p with
  | ExprProgram(_,e) -> eval_expr e
  | _ -> raise (UnimplementedProgram(p))


(* evaluate a value *)
and eval_expr (e:expr_t) : value_t =  match e with
  | ValExpr(p,v) -> v
  | PrintExpr(_, e1) -> 
     let _ = (let v1 = eval_expr e1 in
     Printf.printf "console.log(%s)\n" (str_value v1)) in
     UndefVal

  (*unary operators*) 
  | UopExpr(_,NegUop,e) ->
     NumVal(-. (to_num (eval_expr e)))
  | UopExpr(_,NotUop,e) ->
     if(to_bool (eval_expr e)) then BoolVal(false)
     else BoolVal(true)

  (* MinusBop provided as an example *)
  (* Binary Operators *)
  | BopExpr(_, e1, MinusBop, e2) ->
    (match (eval_expr e1, eval_expr e2) with
    | (NumVal(n1), NumVal(n2)) -> NumVal(n1 -. n2)
    | (StrVal(s1), StrVal(s2)) -> StrVal(remove_substring s1 s2)
    | _ -> UndefVal)
  | BopExpr(_,e1,PlusBop,e2) ->
    (match (eval_expr e1, eval_expr e2) with
    | (NumVal(n1), NumVal(n2)) -> NumVal(n1 +. n2)
    | (StrVal(s1), StrVal(s2)) -> StrVal(s1 ^ " " ^ s2)
    | _ -> UndefVal)
  | BopExpr(_,e1,TimesBop,e2) ->
    NumVal(to_num (eval_expr e1) *. to_num (eval_expr e2))
  | BopExpr(_,e1,DivBop,e2) ->
    NumVal(to_num (eval_expr e1) /. to_num (eval_expr e2))
  | BopExpr(_,e1,EqBop,e2) ->
    BoolVal(to_str (eval_expr e1) = to_str (eval_expr e2))
  | BopExpr(_,e1,NeqBop,e2) ->
    BoolVal(to_str (eval_expr e1) <> to_str (eval_expr e2))
  | BopExpr(_,e1,GtBop,e2) ->
    BoolVal(to_str (eval_expr e1) > to_str (eval_expr e2))
  | BopExpr(_,e1,GteBop,e2) ->
    BoolVal(to_str (eval_expr e1) >= to_str (eval_expr e2))
  | BopExpr(_,e1,LtBop,e2) ->
    BoolVal(to_str (eval_expr e1) < to_str (eval_expr e2))
  | BopExpr(_,e1,LteBop,e2) ->
    BoolVal(to_str (eval_expr e1) <= to_str (eval_expr e2))
  | BopExpr(_,e1,AndBop,e2) ->
    BoolVal(to_bool (eval_expr e1) && to_bool (eval_expr e2))
  | BopExpr(_, e1, OrBop, e2) ->
    let v1 = eval_expr e1 in
    if to_bool v1 then v1 else eval_expr e2

  (* other expression types unimplemented *)
  | _ -> raise (UnimplementedExpr(e))

and remove_substring s1 s2 =
  let len_s2 = String.length s2 in
  let rec remove_anywhere s1 s2 =
    if String.length s1 >= len_s2 then
      let idx = try Some (String.index s1 s2.[0]) with Not_found -> None in
      match idx with
      | Some i -> remove_anywhere (String.sub s1 0 i ^ String.sub s1 (i + len_s2) (String.length s1 - (i + len_s2))) s2
      | None -> s1
    else
      s1
  in
  remove_anywhere s1 s2

(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, compose eval parse_string, eq_value, eq_exn,
   Some((fun (x:string) -> x),str_value), tests)


(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests =
  test_group "Simple Expression Evaluation"
    [
      (None, "1 + true",                    Ok(NumVal(2.0)));
      (None, "false + true",                Ok(NumVal(1.0)));
      (None, "100 || 200",                  Ok(NumVal(100.0)));
      (None, "-false",                      Ok(NumVal(0.0)));
      (None, "1 + 1",                       Ok(NumVal(2.0)));
      (None, "3 + (4 + 5)",                 Ok(NumVal(12.0)));
      (None, "3 * (4 + 5)",                 Ok(NumVal(27.0)));
      (None, "-6 * 90 - 8",                 Ok(NumVal(-548.0)));
      (None, "(-100) + 50",                   Ok(NumVal(-50.0)));
      (None, "true && (false || true)",     Ok(BoolVal(true)));
      (None, "true && (false || !true)",    Ok(BoolVal(false)));
      (None, "1 < 2",                       Ok(BoolVal(true)));
      (None, "100 === 100",                 Ok(BoolVal(true)));
      (None, "100 === 101",                 Ok(BoolVal(false)));
      (None, "100 !== 200",                 Ok(BoolVal(true)));
      (None, "true === true",               Ok(BoolVal(true)));
      (None, "0 / 0",                       Ok(NumVal(nan)));
    ]


let simple_print_eval_tests =
  test_group "Simple Print Evaluation"
    [
      (None, "console.log(\"Hello World\")",           Ok(UndefVal));
    ]

let simple_cond_eval_tests =
  test_group "Simple Conditional Evaluation"
    [
      (None, "(1 < 2) ? 123 : 124",         Ok(NumVal(123.0)));
    ]

let simple_str_eval_tests =
  test_group "Simple String Evaluation"
    [
      (None, "\"aaa\" < \"aaaa\"",          Ok(BoolVal(true)));
      (None, "\"bbb\" < \"aaa\"",           Ok(BoolVal(false)));
      (None, "\"hello\" + \"world\"",   Ok(StrVal("hello world")));
    ]

let eval_tests =
  test_group "Evaluator"
    [
      (None, "1+6+7", Ok(NumVal(14.0))); 
      (None, "5*6/3", Ok(NumVal(10.0)));
      (None, "(10-5)*2", Ok(NumVal(10.0)));
      (None, "7*7*7*7 - 3", Ok(NumVal(2398.0)));
      (None, "2.5 + 1.7 - .3", Ok(NumVal(3.9)));
    ]

let cond_eval_tests =
  test_group "Conditional Evaluation"
    [
      (* TODO *)
    ]

let str_eval_tests =
  test_group "String Evaluation"
    [
      (None, "\"aaaa\" <= \"aaaa\"",          Ok(BoolVal(true)));
      (None, "\"aaaa\" >= \"bbbb\"",          Ok(BoolVal(false)));
      (None, "\"hello\"-\"llo\"",   Ok(StrVal("he")));
      (None, "\"hello\"-\"he\"",   Ok(StrVal("llo")));
      (None, "\"he\"-\"he\"",   Ok(StrVal("")));
      (None, "\"help\"+\"me\"",   Ok(StrVal("help me")));
      (None, "\"he\"===\"he\"",   Ok(BoolVal(true)));
      (None, "\"h\"-\"e\"",   Ok(StrVal("h")));
    ]
