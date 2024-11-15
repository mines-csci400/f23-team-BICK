open Javascript_parser;;
open Javascript_lexer;;
open Javascript_main;;
open Javascript_ast;;
open Util;;

let lexer_test_fun s =
  tokenize (Lexing.from_string s)


let lexer_tests =
  ("lexer_tests",
   lexer_test_fun,
   (=), (=),
   Some((fun (x : string) -> x), str_token_list),
   [
     (Some("simple js"),
      "1 + 2",
      Ok([NUMBER(1.0); ADD_OP; NUMBER(2.0); EOF]));
     (Some("simple js sub"),
      "1 - 2",
      Ok([NUMBER(1.0); SUB_OP; NUMBER(2.0); EOF]));
      (Some("simple js mul"),
      "1 * 2",
      Ok([NUMBER(1.0); MUL_OP; NUMBER(2.0); EOF]));
      (Some("simple js div"),
      "1 / 2",
      Ok([NUMBER(1.0); DIV_OP; NUMBER(2.0); EOF]));
      (Some("simple equation"),
      "1 + 1 = 2",
      Ok([NUMBER(1.0); ADD_OP; NUMBER(1.0); ASSIGN_OP; NUMBER(2.0); EOF]));
      (Some("simple equation 2"),
      "2 * 3 < 9",
      Ok([NUMBER(2.0); MUL_OP; NUMBER(3.0); LT_OP; NUMBER(9.0); EOF]));
      (Some("complex equation"),
      "(5 * 2) / (2 + 3) = 10",
      Ok([LP_KW; NUMBER(5.0); MUL_OP; NUMBER(2.0); RP_KW; DIV_OP; LP_KW; NUMBER(2.0); ADD_OP; NUMBER(3.0); RP_KW; ASSIGN_OP; NUMBER(10.0); EOF]));
      (Some("complex equation 2"),
      "( 2 * ( 3 + 5)) / (( 2 * 6) / 3) <= 5",
      Ok([LP_KW; NUMBER(2.0); MUL_OP; LP_KW; NUMBER(3.0); ADD_OP; NUMBER(5.0); RP_KW; RP_KW; DIV_OP; LP_KW; LP_KW; NUMBER(2.0); MUL_OP; NUMBER(6.0); RP_KW; DIV_OP; NUMBER(3.0); RP_KW; LEQ_OP; NUMBER(5.0); EOF]));
      (Some("hello world"),
      "Hello, world!",
      Ok([IDENT("Hello"); COMMA_OP; IDENT("world"); LOG_NOT_OP; EOF]));
      (Some("long string"),
      "Hey! We are team BICK here doing Lab 5, please give us a good grade",
      Ok([IDENT("Hey"); LOG_NOT_OP; IDENT("We"); IDENT("are"); IDENT("team"); IDENT("BICK"); IDENT("here"); IDENT("doing"); IDENT("Lab"); NUMBER(5.0); COMMA_OP; IDENT("please"); IDENT("give"); IDENT("us"); IDENT("a"); IDENT("good"); IDENT("grade"); EOF]));
      (Some("line of code"),
      "if x === 1 then y = 2;",
      Ok([IDENT("if"); IDENT("x"); STREQ_OP; NUMBER(1.0); IDENT("then"); IDENT("y"); ASSIGN_OP; NUMBER(2.0); SEMICOLON_OP; EOF]));

   ])

let parser_tests =
  ("parser_tests",
   parse_string,
   eq_program, (=),
   Some((fun (x : string) -> x), str_program),
   [
      (Some("simple expression"),
        "1 + 2",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       ValExpr(NoPos, NumVal(1.0)),
                                       PlusBop,
                                       ValExpr(NoPos, NumVal(2.0)) ))));

      (Some("simple string expression"),
        "a + b",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       VarExpr(NoPos, "a"),
                                       PlusBop,
                                       VarExpr(NoPos, "b") ))));

      (Some("simple subtraction"),
        "10 - 3",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       ValExpr(NoPos, NumVal(10.0)),
                                       MinusBop,
                                       ValExpr(NoPos, NumVal(3.0)) ))));
       
      (Some("simple multiplication"),
        "3 * 4",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       ValExpr(NoPos, NumVal(3.0)),
                                       TimesBop,
                                       ValExpr(NoPos, NumVal(4.0)) ))));

      (Some("simple division"),
        "15 / 5",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       ValExpr(NoPos, NumVal(15.0)),
                                       DivBop,
                                       ValExpr(NoPos, NumVal(5.0)) ))));

      (Some("Identity lambda"),
        "function (x) {return x;}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, VarExpr(NoPos, "x")), (* block_t *)
                                 None (* typ_t option *)
                                )))));

      (Some("Function returning function"),
        "function (x) {return x + 1;}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, BopExpr(NoPos, VarExpr(NoPos, "x"), PlusBop, ValExpr(NoPos, NumVal(1.0)))), (* block_t *)
                                 None (* typ_t option *)
                                )))));

      (Some("Param addition in function, multiple parameters"),
        "function (x, y) { return (x + y);}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None); ("y", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, BopExpr(NoPos, VarExpr(NoPos, "x"), PlusBop, VarExpr(NoPos, "y"))), (* block_t *)
                                 None (* typ_t option *)
                                )))));

      (Some("Param addition and multiplication in function with ()"),
        "function (x, y, z) { return (x + (y * z));}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None); ("y", None); ("z", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, BopExpr(NoPos, VarExpr(NoPos, "x"), PlusBop, BopExpr(NoPos, VarExpr(NoPos, "y"), TimesBop, VarExpr(NoPos, "z")))), (* block_t *)
                                 None (* typ_t option *)
                                )))));

      (Some("Multiple params in function, no (), test order of ops"),
        "function (x, y, z) { return (x * y - z);}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None); ("y", None); ("z", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, BopExpr(NoPos, BopExpr(NoPos, VarExpr(NoPos, "x"), TimesBop, VarExpr(NoPos, "y")), MinusBop, VarExpr(NoPos, "z"))), (* block_t *)
                                 None (* typ_t option *)
                                )))));

      (Some("Function with multiple params returning param"),
        "function (x, y, z) {return z;}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None); ("y", None); ("z", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, VarExpr(NoPos, "z")), (* block_t *)
                                 None (* typ_t option *)
                                ))))); 
  ])
