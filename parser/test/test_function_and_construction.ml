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
  let ast = Util.parse_expr {| X Y z |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Construct (constructor ((name X)))
         (payload (Construct (constructor ((name Y))) (payload (Id ((name z)))))))))
      (errors ())))
    |}]
;;

let%expect_test "func apply and construction" =
  let ast = Util.parse_expr {| foo X Y alpha Z bar beta |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Call
         (caller
          (Call
           (caller
            (Call (caller (Id ((name foo))))
             (arg
              (Construct (constructor ((name X)))
               (payload
                (Construct (constructor ((name Y)))
                 (payload (Id ((name alpha))))))))))
           (arg (Construct (constructor ((name Z))) (payload (Id ((name bar))))))))
         (arg (Id ((name beta)))))))
      (errors ())))
    |}]
;;
