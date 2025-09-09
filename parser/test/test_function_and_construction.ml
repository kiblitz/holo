open! Core
open! Import

let%expect_test "func apply" =
  let ast = Util.parse_expr {| x y z |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Call (caller (Call (caller (Id ((name x)))) (arg (Id ((name y))))))
         (arg (Id ((name z)))))))
      (errors ())))
    |}]
;;

let%expect_test "construction" =
  let ast = Util.parse_expr {| X Y Z |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Variant (tag ((name X)))
         (payload
          ((Variant (tag ((name Y)))
            (payload ((Variant (tag ((name Z))) (payload ()))))))))))
      (errors ())))
    |}]
;;

let%expect_test "func apply and construction" =
  let ast = Util.parse_expr {| _ = foo X Y alpha Z bar beta in () |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope (binding (Pattern Underscore))
         (to_
          (Call
           (caller
            (Call
             (caller
              (Call (caller (Id ((name foo))))
               (arg
                (Variant (tag ((name X)))
                 (payload
                  ((Variant (tag ((name Y))) (payload ((Id ((name alpha))))))))))))
             (arg (Variant (tag ((name Z))) (payload ((Id ((name bar)))))))))
           (arg (Id ((name beta))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;

let%expect_test "func define" =
  let ast = Util.parse_expr {| foo x y z = bar in foo 1 2 3 |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope
         (binding
          (Function (id ((name foo)))
           (args ((Id ((name x))) (Id ((name y))) (Id ((name z)))))))
         (to_ (Id ((name bar))))
         (in_
          (Call
           (caller
            (Call
             (caller
              (Call (caller (Id ((name foo))))
               (arg (Constant (Constant (Int 1))))))
             (arg (Constant (Constant (Int 2))))))
           (arg (Constant (Constant (Int 3)))))))))
      (errors ())))
    |}]
;;

let%expect_test "func define with construction" =
  let ast = Util.parse_expr {| foo () (Alpha a) Beta = bar in foo () (Alpha 1) Beta |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope
         (binding
          (Function (id ((name foo)))
           (args
            (Unit (Variant (tag ((name Alpha))) (payload ((Id ((name a))))))
             (Variant (tag ((name Beta))) (payload ()))))))
         (to_ (Id ((name bar))))
         (in_
          (Call
           (caller
            (Call
             (caller (Call (caller (Id ((name foo)))) (arg (Constant Unit))))
             (arg
              (Variant (tag ((name Alpha)))
               (payload ((Constant (Constant (Int 1)))))))))
           (arg (Variant (tag ((name Beta))) (payload ()))))))))
      (errors ())))
    |}]
;;
