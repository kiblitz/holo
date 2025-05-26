open! Core

let%expect_test _ =
  Jag_lib.test ();
  [%expect {| "hello world" |}]
;;
