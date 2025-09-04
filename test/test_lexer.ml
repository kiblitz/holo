open! Core
open! Import

let%expect_test _ =
  let x = Token.Symbol Walrus in
  print_s [%message (x : Token.t)];
  [%expect {| (x (Symbol Walrus)) |}]
;;
