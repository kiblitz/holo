open! Core
open! Import
open! Regex_dfa.For_testing

let to_dfa = Regex_dfa.create ~cont_of_match:(Fn.const ())

let%expect_test "exact dfa" =
  let dfa = Regex_config.exact "foo " |> to_dfa in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((0 (Node (next_nodes ((f 1))))) (1 (Node (next_nodes ((o 2)))))
      (2 (Node (next_nodes ((o 3))))) (3 (Node (next_nodes ((" " 4)))))
      (4
       (Accept (next_nodes ())
        (accepting_state_metadata ((cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "or dfa" =
  let dfa = Regex_config.char_or [ 'b'; 'a'; 'r' ] |> to_dfa in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((5 (Node (next_nodes ((a 6) (b 6) (r 6)))))
      (6
       (Accept (next_nodes ())
        (accepting_state_metadata ((cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: floats" =
  let dfa = to_dfa Util.Common_config.float in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((7
       (Node
        (next_nodes
         ((0 8) (1 8) (2 8) (3 8) (4 8) (5 8) (6 8) (7 8) (8 8) (9 8)))))
      (8
       (Node
        (next_nodes
         ((. 9) (0 8) (1 8) (2 8) (3 8) (4 8) (5 8) (6 8) (7 8) (8 8) (9 8)))))
      (9
       (Accept
        (next_nodes
         ((0 9) (1 9) (2 9) (3 9) (4 9) (5 9) (6 9) (7 9) (8 9) (9 9)))
        (accepting_state_metadata ((cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((10
       (Node
        (next_nodes
         (("(" 11) (+ 25) (0 28) (1 28) (2 28) (3 28) (4 28) (5 28) (6 28)
          (7 28) (8 28) (9 28)))))
      (11
       (Node
        (next_nodes
         ((0 12) (1 12) (2 12) (3 12) (4 12) (5 12) (6 12) (7 12) (8 12) (9 12)))))
      (12
       (Node
        (next_nodes
         ((0 13) (1 13) (2 13) (3 13) (4 13) (5 13) (6 13) (7 13) (8 13) (9 13)))))
      (13
       (Node
        (next_nodes
         ((0 14) (1 14) (2 14) (3 14) (4 14) (5 14) (6 14) (7 14) (8 14) (9 14)))))
      (14 (Node (next_nodes ((")" 15))))) (15 (Node (next_nodes ((" " 16)))))
      (16
       (Node
        (next_nodes
         ((0 17) (1 17) (2 17) (3 17) (4 17) (5 17) (6 17) (7 17) (8 17) (9 17)))))
      (17
       (Node
        (next_nodes
         ((0 18) (1 18) (2 18) (3 18) (4 18) (5 18) (6 18) (7 18) (8 18) (9 18)))))
      (18
       (Node
        (next_nodes
         ((0 19) (1 19) (2 19) (3 19) (4 19) (5 19) (6 19) (7 19) (8 19) (9 19)))))
      (19 (Node (next_nodes ((- 20)))))
      (20
       (Node
        (next_nodes
         ((0 21) (1 21) (2 21) (3 21) (4 21) (5 21) (6 21) (7 21) (8 21) (9 21)))))
      (21
       (Node
        (next_nodes
         ((0 22) (1 22) (2 22) (3 22) (4 22) (5 22) (6 22) (7 22) (8 22) (9 22)))))
      (22
       (Node
        (next_nodes
         ((0 23) (1 23) (2 23) (3 23) (4 23) (5 23) (6 23) (7 23) (8 23) (9 23)))))
      (23
       (Node
        (next_nodes
         ((0 24) (1 24) (2 24) (3 24) (4 24) (5 24) (6 24) (7 24) (8 24) (9 24)))))
      (24
       (Accept (next_nodes ())
        (accepting_state_metadata ((cont_of_match <fun>)))))
      (25
       (Node
        (next_nodes
         ((0 26) (1 26) (2 26) (3 26) (4 26) (5 26) (6 26) (7 26) (8 26) (9 26)))))
      (26
       (Node
        (next_nodes
         ((" " 27) (0 26) (1 26) (2 26) (3 26) (4 26) (5 26) (6 26) (7 26)
          (8 26) (9 26)))))
      (27
       (Node
        (next_nodes
         (("(" 11) (0 28) (1 28) (2 28) (3 28) (4 28) (5 28) (6 28) (7 28)
          (8 28) (9 28)))))
      (28
       (Node
        (next_nodes
         ((0 29) (1 29) (2 29) (3 29) (4 29) (5 29) (6 29) (7 29) (8 29) (9 29)))))
      (29
       (Node
        (next_nodes
         ((0 30) (1 30) (2 30) (3 30) (4 30) (5 30) (6 30) (7 30) (8 30) (9 30)))))
      (30
       (Node
        (next_nodes
         ((- 16) (0 31) (1 31) (2 31) (3 31) (4 31) (5 31) (6 31) (7 31)
          (8 31) (9 31)))))
      (31
       (Node
        (next_nodes
         ((0 32) (1 32) (2 32) (3 32) (4 32) (5 32) (6 32) (7 32) (8 32) (9 32)))))
      (32
       (Node
        (next_nodes
         ((0 20) (1 20) (2 20) (3 20) (4 20) (5 20) (6 20) (7 20) (8 20) (9 20)))))))
    |}]
;;
