open! Core
open! Import

let%expect_test "expr" =
  let ast = Util.parse_program {| (x + y, z - 2 ** 14), foo bar |} in
  print_s [%message (ast : Ast.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        ((toplevel
          ((Tuple
            (children
             ((Tuple
               (children
                ((Infix (symbol (Base (Plus))) (left_t (Id ((name x))))
                  (right_t (Id ((name y)))))
                 (Infix (symbol (Base (Minus))) (left_t (Id ((name z))))
                  (right_t
                   (Infix (symbol (Base (Times Times)))
                    (left_t (Constant (Constant (Int 2))))
                    (right_t (Constant (Constant (Int 14))))))))))
              (Call (caller (Id ((name foo)))) (arg (Id ((name bar)))))))))))))
      (errors ())))
    |}]
;;

let%expect_test "simple program" =
  let ast =
    Util.parse_program
      {|
z - 2 ** 14;
- foo bar;
buah [(alpha, beta)];
|}
  in
  print_s [%message (ast : Ast.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        ((toplevel
          ((Infix (symbol (Base (Minus))) (left_t (Id ((name z))))
            (right_t
             (Infix (symbol (Base (Times Times)))
              (left_t (Constant (Constant (Int 2))))
              (right_t (Constant (Constant (Int 14)))))))
           (Prefix (symbol (Minus))
            (t (Call (caller (Id ((name foo)))) (arg (Id ((name bar)))))))
           (Call (caller (Id ((name buah))))
            (arg
             (List
              (children
               ((Tuple (children ((Id ((name alpha))) (Id ((name beta))))))))))))))))
      (errors ())))
    |}]
;;
