open! Core
open! Import

let parse text =
  let%map.With_errors tokens = Lexer.lex text ~filename:"file.txt" in
  Parser.parse tokens
;;

let%expect_test "func apply" =
  let ast = parse {| x y z |} in
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

let%expect_test "operator precedence" =
  let ast = parse {| x + y * z ** foo bar 2 |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Infix (symbol (Base (Plus))) (left_t (Id ((name x))))
         (right_t
          (Infix (symbol (Base (Times))) (left_t (Id ((name y))))
           (right_t
            (Infix (symbol (Base (Times Times))) (left_t (Id ((name z))))
             (right_t
              (Call
               (caller (Call (caller (Id ((name foo)))) (arg (Id ((name bar))))))
               (arg (Constant (Constant (Int 2)))))))))))))
      (errors ())))
    |}]
;;

let%expect_test "operator precedence with parens" =
  let ast = parse {| x + (y * z) ** foo (bar 2) |} in
  print_s [%message (ast : Ast.Expr.t Or_error.t With_errors.t)];
  [%expect
    {|
    (ast
     ((value
       (Ok
        (Infix (symbol (Base (Plus))) (left_t (Id ((name x))))
         (right_t
          (Infix (symbol (Base (Times Times)))
           (left_t
            (Infix (symbol (Base (Times))) (left_t (Id ((name y))))
             (right_t (Id ((name z))))))
           (right_t
            (Call (caller (Id ((name foo))))
             (arg
              (Call (caller (Id ((name bar))))
               (arg (Constant (Constant (Int 2)))))))))))))
      (errors ())))
    |}]
;;

let%expect_test "tuple" =
  let ast = parse {| x, y, z |} in
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
  let ast = parse {| (u, v), (w, x, (y, z)) |} in
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
  let ast = parse {| x + (y * z) ** foo, alpha (beta, bar 2) |} in
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
