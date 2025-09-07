open! Core
open! Import

let%expect_test "tuple" =
  let ast = Util.parse_expr {| x, y, z |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok (Tuple (children ((Id ((name x))) (Id ((name y))) (Id ((name z))))))))
      (errors ())))
    |}]
;;

let%expect_test "tuple mania" =
  let ast = Util.parse_expr {| (u, v), (w, x, (y, z)) |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Tuple
         (children
          ((Tuple (children ((Id ((name u))) (Id ((name v))))))
           (Tuple
            (children
             ((Id ((name w))) (Id ((name x)))
              (Tuple (children ((Id ((name y))) (Id ((name z))))))))))))))
      (errors ())))
    |}]
;;

let%expect_test "operator precedence with tuples" =
  let ast = Util.parse_expr {| x + (y * z) ** foo, alpha (beta, bar 2) |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Tuple
         (children
          ((Infix (symbol (Base (Plus))) (left_t (Id ((name x))))
            (right_t
             (Infix (symbol (Base (Times Times)))
              (left_t
               (Infix (symbol (Base (Times))) (left_t (Id ((name y))))
                (right_t (Id ((name z))))))
              (right_t (Id ((name foo)))))))
           (Call (caller (Id ((name alpha))))
            (arg
             (Tuple
              (children
               ((Id ((name beta)))
                (Call (caller (Id ((name bar))))
                 (arg (Constant (Constant (Int 2)))))))))))))))
      (errors ())))
    |}]
;;
