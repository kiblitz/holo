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
     ((413 (Node (next_nodes ((f 414))))) (414 (Node (next_nodes ((o 415)))))
      (415 (Node (next_nodes ((o 416))))) (416 (Node (next_nodes ((" " 417)))))
      (417
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "or dfa" =
  let dfa = Regex_config.char_or [ 'b'; 'a'; 'r' ] |> to_dfa in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((418 (Node (next_nodes ((a 419) (b 419) (r 419)))))
      (419
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: floats" =
  let dfa = to_dfa Util.Common_config.float in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((420
       (Node
        (next_nodes
         ((0 421) (1 421) (2 421) (3 421) (4 421) (5 421) (6 421) (7 421)
          (8 421) (9 421)))))
      (421
       (Node
        (next_nodes
         ((. 422) (0 421) (1 421) (2 421) (3 421) (4 421) (5 421) (6 421)
          (7 421) (8 421) (9 421)))))
      (422
       (Accept
        (next_nodes
         ((0 422) (1 422) (2 422) (3 422) (4 422) (5 422) (6 422) (7 422)
          (8 422) (9 422)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((423
       (Node
        (next_nodes
         (("(" 424) (+ 438) (0 441) (1 441) (2 441) (3 441) (4 441) (5 441)
          (6 441) (7 441) (8 441) (9 441)))))
      (424
       (Node
        (next_nodes
         ((0 425) (1 425) (2 425) (3 425) (4 425) (5 425) (6 425) (7 425)
          (8 425) (9 425)))))
      (425
       (Node
        (next_nodes
         ((0 426) (1 426) (2 426) (3 426) (4 426) (5 426) (6 426) (7 426)
          (8 426) (9 426)))))
      (426
       (Node
        (next_nodes
         ((0 427) (1 427) (2 427) (3 427) (4 427) (5 427) (6 427) (7 427)
          (8 427) (9 427)))))
      (427 (Node (next_nodes ((")" 428))))) (428 (Node (next_nodes ((" " 429)))))
      (429
       (Node
        (next_nodes
         ((0 430) (1 430) (2 430) (3 430) (4 430) (5 430) (6 430) (7 430)
          (8 430) (9 430)))))
      (430
       (Node
        (next_nodes
         ((0 431) (1 431) (2 431) (3 431) (4 431) (5 431) (6 431) (7 431)
          (8 431) (9 431)))))
      (431
       (Node
        (next_nodes
         ((0 432) (1 432) (2 432) (3 432) (4 432) (5 432) (6 432) (7 432)
          (8 432) (9 432)))))
      (432 (Node (next_nodes ((- 433)))))
      (433
       (Node
        (next_nodes
         ((0 434) (1 434) (2 434) (3 434) (4 434) (5 434) (6 434) (7 434)
          (8 434) (9 434)))))
      (434
       (Node
        (next_nodes
         ((0 435) (1 435) (2 435) (3 435) (4 435) (5 435) (6 435) (7 435)
          (8 435) (9 435)))))
      (435
       (Node
        (next_nodes
         ((0 436) (1 436) (2 436) (3 436) (4 436) (5 436) (6 436) (7 436)
          (8 436) (9 436)))))
      (436
       (Node
        (next_nodes
         ((0 437) (1 437) (2 437) (3 437) (4 437) (5 437) (6 437) (7 437)
          (8 437) (9 437)))))
      (437
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (438
       (Node
        (next_nodes
         ((0 439) (1 439) (2 439) (3 439) (4 439) (5 439) (6 439) (7 439)
          (8 439) (9 439)))))
      (439
       (Node
        (next_nodes
         ((" " 440) (0 439) (1 439) (2 439) (3 439) (4 439) (5 439) (6 439)
          (7 439) (8 439) (9 439)))))
      (440
       (Node
        (next_nodes
         (("(" 424) (0 441) (1 441) (2 441) (3 441) (4 441) (5 441) (6 441)
          (7 441) (8 441) (9 441)))))
      (441
       (Node
        (next_nodes
         ((0 442) (1 442) (2 442) (3 442) (4 442) (5 442) (6 442) (7 442)
          (8 442) (9 442)))))
      (442
       (Node
        (next_nodes
         ((0 443) (1 443) (2 443) (3 443) (4 443) (5 443) (6 443) (7 443)
          (8 443) (9 443)))))
      (443
       (Node
        (next_nodes
         ((- 429) (0 444) (1 444) (2 444) (3 444) (4 444) (5 444) (6 444)
          (7 444) (8 444) (9 444)))))
      (444
       (Node
        (next_nodes
         ((0 445) (1 445) (2 445) (3 445) (4 445) (5 445) (6 445) (7 445)
          (8 445) (9 445)))))
      (445
       (Node
        (next_nodes
         ((0 433) (1 433) (2 433) (3 433) (4 433) (5 433) (6 433) (7 433)
          (8 433) (9 433)))))))
    |}]
;;
