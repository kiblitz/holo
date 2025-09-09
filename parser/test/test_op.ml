open! Core
open! Import

let%expect_test "op overload" =
  let ast = Util.parse_expr {| op @ x y = x * y in x @ y |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Scope
         (binding
          (Op_overload (op (Base (At))) (args ((Id ((name x))) (Id ((name y)))))))
         (to_
          (Infix (symbol (Base (Times))) (left_t (Id ((name x))))
           (right_t (Id ((name y))))))
         (in_
          (Infix (symbol (Base (At))) (left_t (Id ((name x))))
           (right_t (Id ((name y)))))))))
      (errors ())))
    |}]
;;

let%expect_test "op as function" =
  let ast = Util.parse_expr {| _ = op @ x y in () |} in
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
            (Call (caller (Op (symbol (Base (At))))) (arg (Id ((name x))))))
           (arg (Id ((name y))))))
         (in_ (Constant Unit)))))
      (errors ())))
    |}]
;;
