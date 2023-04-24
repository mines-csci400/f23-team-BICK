open Javascript_ast
open Javascript_heap
open Javascript_main
open Util

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 * - read_environment        -- look up a variable's value in the environment
 * - bind_environment        -- add a variable binding to the environment
 * - empty_env               -- the empty environment
 *)

(*
 * Check javascript_heap.ml for the following useful functionality:
 * -  heap_alloc             -- allocate a new reference cell
 * -  heap_assign            -- update the value in a reference cell
 * -  heap_deref             -- retrieve the value from a reference
 *)


(* evaluate a program *)
let rec eval (env : environment_t) (h:heap_t) (p: program_t) : value_t*heap_t =
  match p with
  | ExprProgram(_,e) -> eval_expr env h e
  (* TODO *)
  | _ -> raise (UnimplementedProgram(p))

(* evaluate a block *)
and eval_block (env:environment_t) (h:heap_t) (p:block_t) : value_t*heap_t =
  match p with
  | ReturnBlock(_,e) -> eval_expr env h e
  (* TODO *)
  | _ -> raise (UnimplementedBlock(p))

(* evaluate a statement *)
and eval_stmt (env:environment_t) (h:heap_t) (s:stmt_t) : environment_t*heap_t =
  match s with
  (* TODO *)
  | _ -> raise (UnimplementedStmt(s))

(* evaluate a value *)
and eval_expr (env:environment_t) (h:heap_t) (e:expr_t) : value_t*heap_t =
  (*Printf.printf "%s %s\n" (str_expr e) (str_environment_simple env);*)
  match e with
  | BlockExpr(p,b) -> eval_block env h b
  | BopExpr(_,e1,MinusBop,e2) ->
     let v1,h = (eval_expr env h e1) in
     let v2,h = (eval_expr env h e2) in
     NumVal(to_num v1  -. to_num v2 ),h
  (* TODO *)
  | _ -> raise (UnimplementedExpr(e))


(*********)
(* Tests *)
(*********)

let test_group name tests =
  (name, compose (eval empty_env empty_heap) parse_string, eq_value_heap_value, eq_exn,
   Some((fun (x : string) -> x),
        (fun ((v,h) : value_t*heap_t) -> str_value v)),
   (* None, *)
   (List.map
      (fun (name,js,expected) ->
        (name,js,(match expected with
                  | Ok(v) -> Ok(v,empty_heap)
                  | Error(v) -> Error(v))))
     tests))

(* basic tests for the evaluator (do not modify) *)
let simple_expr_eval_tests =
  test_group "Simple Expression Evaluation"
    [
      (None, "1 + true",                     Ok(NumVal(2.0)));
      (None, "false + true",                 Ok(NumVal(1.0)));
      (None, "100 || 200",                   Ok(NumVal(100.0)));
      (None, "-false",                       Ok(NumVal(0.0)));
      (None, "1 + 1",                        Ok(NumVal(2.0)));
      (None, "3 + (4 + 5)",                  Ok(NumVal(12.0)));
      (None, "3 * (4 + 5)",                  Ok(NumVal(27.0)));
      (None, "-6 * 90 - 8",                  Ok(NumVal(-548.0)));
      (None, "-100 + 50",                    Ok(NumVal(-50.0)));
      (None, "true && (false || true)",      Ok(BoolVal(true)));
      (None, "true && (false || !true)",     Ok(BoolVal(false)));
      (None, "1 < 2",                        Ok(BoolVal(true)));
      (None, "100 === 100",                  Ok(BoolVal(true)));
      (None, "100 === 101",                  Ok(BoolVal(false)));
      (None, "100 !== 200",                  Ok(BoolVal(true)));
      (None, "true === true",                Ok(BoolVal(true)));
      (None, "0 / 0",                        Ok(NumVal(nan)));
      (None, "console.log(\"Hello World\")", Ok(UndefVal));
      (None, "(1 < 2) ? 123 : 124",          Ok(NumVal(123.0)));
      (None, "\"aaa\" < \"aaaa\"",           Ok(BoolVal(true)));
      (None, "\"bbb\" < \"aaa\"",            Ok(BoolVal(false)));
      (None, "\"hello\"+\" \"+\"world\"",    Ok(StrVal("hello world")));
      (None, "const x = 1; x+1",             Ok(NumVal(2.0)));
      (None, "const x=1; const y=2; x+y",    Ok(NumVal(3.0)));
      (None, "const x=3; const y=x*2+1; y",  Ok(NumVal(7.0)));
    ]

let fact_js = "function factorial(n){return (n <= 1) ? 1 : (n * factorial(n-1));}"
let fib_js = "function fib(x){return x<=0 ? 0 : (x===1 ? 1 : fib(x-1)+fib(x-2));}"
let scopes_js =
"(function (x) {
    return function(f) {
        return function (x) {
            return f(0);
        }(2);
    } (function (y) {return x;});
} (1))"

let readme1_js =
  "const f = function(x){ return x+1; };
   const r = f(2);
   r+3"

let readme2_js =
  "const x = 5;
   const f = function(y){ return x + y; };
   (function(z) { const x = 7; return f(6); })(0)"

(* basic tests for the evaluator (do not modify) *)
let simple_func_eval_tests =
  test_group "Simple Function Definition Evaluation"
    [
      (None, "function test(x){const x = 123; return x;}",
       Ok(ClosureVal(StringMap.empty,(
                Some("test"),
                [("x",None)],
                StmtBlock(NoPos,
                          ConstStmt(NoPos,
                                    "x",
                                    ValExpr(NoPos,NumVal(123.0))),
                          ReturnBlock(NoPos,VarExpr(NoPos,"x"))),
                None))));
      (None, fact_js,
       Ok(ClosureVal(StringMap.empty,(
                Some("factorial"),
                [("n",None)],
                ReturnBlock(NoPos,IfExpr(NoPos,
                                         BopExpr(NoPos,VarExpr(NoPos,"n"),LteBop,ValExpr(NoPos,NumVal(1.0))),
                                         ValExpr(NoPos,NumVal(1.0)),
                                         BopExpr(NoPos,VarExpr(NoPos,"n"),TimesBop,CallExpr(NoPos,VarExpr(NoPos,"factorial"),[BopExpr(NoPos,VarExpr(NoPos,"n"),MinusBop,ValExpr(NoPos,NumVal(1.0)))]))
                  )),
                None))));

    ]
(* note - you can use the following to print a program for debugging *)
(* let _ = Printf.printf "RESULT = %s\n" (str_program (parse_string "const x = 1 + 1; x * 2")) *)


let simple_call_eval_tests =
  test_group "Simple Call Evaluation"
    [
      (None, "const f = function(x){return x+1;}; f(1)",                    Ok(NumVal(2.0)));
      (None, "const y = 5; const f = function(x){return x+1;}; f(y)",       Ok(NumVal(6.0)));
      (Some("recursion"), "const f = function t(x){return x===0 ? 0 : x+t(x-1);}; f(5)", Ok(NumVal(15.0)));

      (Some("Readme 1"), readme1_js, Ok(NumVal(6.0)));
      (Some("Readme 2"), readme2_js, Ok(NumVal(11.0)));
      (Some("Lecture Scoping Test"), scopes_js, Ok(NumVal(1.0)))
    ]


(** Basic (instructor-provided) tests for mutability (do not modify.) *)
let simple_mut_eval_tests =
  test_group "Simple Mutability Evaluation"
    [
      (None, "1 = 100; 0",                   Error(RefError(ValExpr(NoPos,(NumVal(1.0))))));
      (None, "const x=3; x=1; x+1",          Error(ImmutableVar("x")));
      (None, "let x=3; x+1",                 Ok(NumVal(4.0)));
      (None, "let x=3; x=1; x+1",            Ok(NumVal(2.0)));
      (None, "let x=3; let y=4; x=x+y+5; y=10+y+x; x+y",            Ok(NumVal(38.0)));
      (None, "let x=3; x=true; x",            Ok(BoolVal(true)));
      (Some("static scope"), "let x=5; const f=function(y){return x+y;}; x=10; (function(z){const x=7; return f(6);})(0)", Ok(NumVal(16.0)));
]

let mut_eval_tests =
  test_group "Mutability Evaluation"
    [
      (* TODO *)
    ]
