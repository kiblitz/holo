open! Core
open! Import

let%expect_test _ =
  let x = Token.Symbol (Non_custom Comma) in
  print_s [%message (x : Token.t)];
  [%expect {| (x (Symbol (Non_custom Comma))) |}]
;;
