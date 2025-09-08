open! Core
open! Import

let%expect_test "singleton list" =
  let ast = Util.parse_expr {| [ 5 ] |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value (Ok (List (children ((Constant (Constant (Int 5)))))))) (errors ())))
    |}]
;;

let%expect_test "list" =
  let ast = Util.parse_expr {| [ x, 5, foo ] |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (List
         (children
          ((Id ((name x))) (Constant (Constant (Int 5))) (Id ((name foo))))))))
      (errors ())))
    |}]
;;

let%expect_test "list of exprs" =
  let ast = Util.parse_expr {| _ = [ x * y, 5 + 6, foo bar ] in () |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope (binding (Recursive (Pattern Underscore)))
         (to_
          (List
           (children
            ((Infix (symbol (Base (Times))) (left_t (Id ((name x))))
              (right_t (Id ((name y)))))
             (Infix (symbol (Base (Plus))) (left_t (Constant (Constant (Int 5))))
              (right_t (Constant (Constant (Int 6)))))
             (Call (caller (Id ((name foo)))) (arg (Id ((name bar)))))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;

let%expect_test "list of a tuple" =
  let ast = Util.parse_expr {| _ = [ (x * y, 5 + 6, foo bar) ] in () |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope (binding (Recursive (Pattern Underscore)))
         (to_
          (List
           (children
            ((Tuple
              (children
               ((Infix (symbol (Base (Times))) (left_t (Id ((name x))))
                 (right_t (Id ((name y)))))
                (Infix (symbol (Base (Plus)))
                 (left_t (Constant (Constant (Int 5))))
                 (right_t (Constant (Constant (Int 6)))))
                (Call (caller (Id ((name foo)))) (arg (Id ((name bar))))))))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;

let%expect_test "list of tuples" =
  let ast = Util.parse_expr {| _ = [ (x * y, 5 + 6), foo bar, (5, 6) ] in () |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope (binding (Recursive (Pattern Underscore)))
         (to_
          (List
           (children
            ((Tuple
              (children
               ((Infix (symbol (Base (Times))) (left_t (Id ((name x))))
                 (right_t (Id ((name y)))))
                (Infix (symbol (Base (Plus)))
                 (left_t (Constant (Constant (Int 5))))
                 (right_t (Constant (Constant (Int 6))))))))
             (Call (caller (Id ((name foo)))) (arg (Id ((name bar)))))
             (Tuple
              (children
               ((Constant (Constant (Int 5))) (Constant (Constant (Int 6))))))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;
