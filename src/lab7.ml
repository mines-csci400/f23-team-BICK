open Javascript_ast
open Javascript_main
open Util

(*
 * Check javascript_ast.ml for the following useful functionality:
 * - str_float               -- convert a float to a string
 * - to_num, to_bool, to_str -- do the JavaScript automatic type conversion
 * - bind_environment        -- add a variable binding to the environment
 * - read_environment        -- look up a variable's value in the environment
 * - empty_env               -- the empty environment
 *)

(*
 * (eval env p) should reduce a program in initial environment env to a *value*
 * In general, if Node.js produces a *value* for an example JavaScript
 * program, your evaluator should produce that same value.
 * You should support basic JavaScript (recursive higher-order) functions
 * with lexical scoping.
 *
 * See the assignment writeup for more details.
 *)

(* evaluate a program *)
let rec eval (env : environment_t) (p: program_t) : value_t = match p with
 | ExprProgram(_,e) -> eval_expr env e
 | StmtProgram(_,s,p') ->
    let new_env = eval_stmt env s in
    eval new_env p'

(* evaluate a block *)
and eval_block (env:environment_t) (p:block_t) : value_t = match p with
  | ReturnBlock(_,e) -> eval_expr env e
  | StmtBlock(_, stmt, rest) ->
      let env' = eval_stmt env stmt in
      eval_block env' rest

(* evaluate a statement *)
and eval_stmt (env:environment_t) (s:stmt_t) : environment_t = match s with
 | ConstStmt(_, v, e) ->
      if StringMap.mem v env then
        raise (ImmutableVar v)
      else
        let value = eval_expr env e in
        bind_environment env v Immutable value

  | LetStmt(_, v, e) ->
      let value = eval_expr env e in
      bind_environment env v Mutable value

  | AssignStmt(_, e1, e2) ->
      let value = eval_expr env e2 in
      let var_name = match e1 with
        | VarExpr(_, v) -> v
        | _ -> raise (RefError e1) in
      let access, _ = read_environment env var_name |> Option.get in
      if access = Mutable then
        bind_environment env var_name Mutable value
      else
        raise (ImmutableVar var_name)

(* evaluate a value *)
and eval_expr (env:environment_t) (e:expr_t) : value_t =  match e with
  | ValExpr(p,v) -> v
  | VarExpr(_, v) ->
      (match read_environment env v with
      | Some (_, value) -> value
      | None -> raise (UndeclaredVar v))

    FuncExpr(_, lambda) ->
      let (maybe_name, params, body, maybe_return_type) = lambda in
      let name = match maybe_name with
        | Some(n) -> n
        | None -> "" (* Handle anonymous functions *)
      in
      ClosureVal(env, Some(name), params, (maybe_return_type, body))
      
  | PrintExpr(_, e1) -> 
     (let _ = (let v1 = eval_expr env e1 in
     Printf.printf "console.log(%s)\n" (str_value v1)) in
     UndefVal)

  (*unary operators*) 
  | UopExpr(_,NegUop,e) ->
      NumVal(-. (to_num (eval_expr env e)))
  | UopExpr(_,NotUop,e) ->
     if(to_bool (eval_expr env e)) then BoolVal(false)
     else BoolVal(true)

  (* MinusBop provided as an example *)
  (* Binary Operators *)
  | BopExpr(_, e1, MinusBop, e2) ->
    (match (eval_expr env e1, eval_expr env e2) with
    | (NumVal(n1), NumVal(n2)) -> NumVal(n1 -. n2)
    | (StrVal(s1), StrVal(s2)) -> StrVal(remove_substring s1 s2)
    | _ -> UndefVal)
  | BopExpr(_,e1,PlusBop,e2) ->
    (match (eval_expr env e1, eval_expr env e2) with
      | (NumVal(n1), NumVal(n2)) -> NumVal(n1 +. n2)
      | (StrVal(s1), StrVal(s2)) -> StrVal(s1 ^ s2)
      | (BoolVal(b), NumVal(n)) | (NumVal(n), BoolVal(b)) -> NumVal(if b then n +. 1.0 else n)
      | (BoolVal(b1), BoolVal(b2)) -> NumVal(float_of_int(if b1 then 1 else 0) +. float_of_int(if b2 then 1 else 0))
      | _ -> UndefVal)
  | BopExpr(_,e1,TimesBop,e2) ->
    NumVal(to_num (eval_expr env e1) *. to_num (eval_expr env e2))
  | BopExpr(_,e1,DivBop,e2) ->
    NumVal(to_num (eval_expr env e1) /. to_num (eval_expr env e2))
  | BopExpr(_,e1,EqBop,e2) ->
    BoolVal(to_str (eval_expr env e1) = to_str (eval_expr env e2))
  | BopExpr(_,e1,NeqBop,e2) ->
    BoolVal(to_str (eval_expr env e1) <> to_str (eval_expr env e2))
  | BopExpr(_,e1,GtBop,e2) ->
    BoolVal(to_str (eval_expr env e1) > to_str (eval_expr env e2))
  | BopExpr(_,e1,GteBop,e2) ->
    BoolVal(to_str (eval_expr env e1) >= to_str (eval_expr env e2))
  | BopExpr(_,e1,LtBop,e2) ->
    BoolVal(to_str (eval_expr env e1) < to_str (eval_expr env e2))
  | BopExpr(_,e1,LteBop,e2) ->
    BoolVal(to_str (eval_expr env e1) <= to_str (eval_expr env e2))
  | BopExpr(_,e1,AndBop,e2) ->
    BoolVal(to_bool (eval_expr env e1) && to_bool (eval_expr env e2))
  | BopExpr(_, e1, OrBop, e2) ->
    let v1 = eval_expr env e1 in
    if to_bool v1 then v1 else eval_expr env e2
  | 
    IfExpr(_, e1, e2, e3) ->
      let cond_val = to_bool (eval_expr env e1) in
      if cond_val then
        eval_expr env e2
      else
        eval_expr env e3


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
  (name, compose (eval empty_env) parse_string, eq_value, eq_exn,
   Some((fun (x : string) -> x),str_value),
   (* None, *)
   tests)

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
    ]

let simple_var_eval_tests =
  test_group "Simple Variable Evaluation"
    [
      (None, "const x = 1; x+1",            Ok(NumVal(2.0)));
      (None, "const x=1; const y=2; x+y",   Ok(NumVal(3.0)));
      (None, "const x=3; const y=x*2+1; y", Ok(NumVal(7.0)));
      (None, "const x = 1; y",              Error(UndeclaredVar("y")));
    ]


let var_eval_tests =
  test_group "Variable Evaluation"
    [
      (None, "const x = 1; const y = 2; x",
        Ok(NumVal(1.0)));
      (None, "const x = 2; const y = -3; x * y",
        Ok(NumVal(-6.0)));
      (None, "const x = 10; const y = 2; x / y",
        Ok(NumVal(5.0)));
      (None, "const x = 4; const y = 2; x / (x - y) + x * y",
        Ok(NumVal(10.0)));
      (None, "const x = 5; const y = 2; x * (x - y)", 
        Ok(NumVal(15.0)));
      (None, "const x = 2; y * 3", 
        Error(UndeclaredVar("y")));
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

let func_eval_tests =
  test_group "Function Definition Evaluation"
    [
      (None, "const x = 123; function(y) {return x + y;}",
       Ok(ClosureVal(StringMap.empty,(
                Some("function including y"),
                [("x",None)],
                StmtBlock(NoPos,
                          ConstStmt(NoPos,
                                    "x",
                                    ValExpr(NoPos,NumVal(123.0))),
                          ReturnBlock(NoPos,VarExpr(NoPos,"x"))),
                None))));
      (None, "const x = 4; function(x) {return x + 5;}",
       Ok(ClosureVal(StringMap.empty,(
                Some("function adding to x"),
                [("x",None)],
                StmtBlock(NoPos,
                          ConstStmt(NoPos,
                                    "x",
                                    ValExpr(NoPos,NumVal(4.0))),
                          ReturnBlock(NoPos,
                                    BopExpr(NoPos, VarExpr(NoPos,"x"), PlusBop, ValExpr(NoPos, NumVal(5.0))))),
                None))));
    ]

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

let call_eval_tests =
  test_group "Call Evaluation"
    [
      (* TODO *)
      (Some("fib"), Printf.sprintf "(%s)(30)" fib_js, Ok(NumVal(832040.0)));
    ]
