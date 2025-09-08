open! Core
open! Import

let%expect_test "operator precedence" =
  let ast = Util.parse_expr {| _ = -x + y * z ** -foo bar 2 in () |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope (binding (Recursive (Pattern Underscore)))
         (to_
          (Infix (symbol (Base (Plus)))
           (left_t (Prefix (symbol (Minus)) (t (Id ((name x))))))
           (right_t
            (Infix (symbol (Base (Times))) (left_t (Id ((name y))))
             (right_t
              (Infix (symbol (Base (Times Times))) (left_t (Id ((name z))))
               (right_t
                (Prefix (symbol (Minus))
                 (t
                  (Call
                   (caller
                    (Call (caller (Id ((name foo)))) (arg (Id ((name bar))))))
                   (arg (Constant (Constant (Int 2))))))))))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;

let%expect_test "operator precedence with parens" =
  let ast = Util.parse_expr {| _ = -x + (y * z) ** -foo (bar 2) in () |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope (binding (Recursive (Pattern Underscore)))
         (to_
          (Infix (symbol (Base (Plus)))
           (left_t (Prefix (symbol (Minus)) (t (Id ((name x))))))
           (right_t
            (Infix (symbol (Base (Times Times)))
             (left_t
              (Infix (symbol (Base (Times))) (left_t (Id ((name y))))
               (right_t (Id ((name z))))))
             (right_t
              (Prefix (symbol (Minus))
               (t
                (Call (caller (Id ((name foo))))
                 (arg
                  (Call (caller (Id ((name bar))))
                   (arg (Constant (Constant (Int 2))))))))))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;
