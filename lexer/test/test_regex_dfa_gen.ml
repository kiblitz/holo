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
     ((416 (Node (next_nodes ((f 417))))) (417 (Node (next_nodes ((o 418)))))
      (418 (Node (next_nodes ((o 419))))) (419 (Node (next_nodes ((" " 420)))))
      (420
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
     ((421 (Node (next_nodes ((a 422) (b 422) (r 422)))))
      (422
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
     ((423
       (Node
        (next_nodes
         ((0 424) (1 424) (2 424) (3 424) (4 424) (5 424) (6 424) (7 424)
          (8 424) (9 424)))))
      (424
       (Node
        (next_nodes
         ((. 425) (0 424) (1 424) (2 424) (3 424) (4 424) (5 424) (6 424)
          (7 424) (8 424) (9 424)))))
      (425
       (Accept
        (next_nodes
         ((0 425) (1 425) (2 425) (3 425) (4 425) (5 425) (6 425) (7 425)
          (8 425) (9 425)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((426
       (Node
        (next_nodes
         (("(" 427) (+ 441) (0 444) (1 444) (2 444) (3 444) (4 444) (5 444)
          (6 444) (7 444) (8 444) (9 444)))))
      (427
       (Node
        (next_nodes
         ((0 428) (1 428) (2 428) (3 428) (4 428) (5 428) (6 428) (7 428)
          (8 428) (9 428)))))
      (428
       (Node
        (next_nodes
         ((0 429) (1 429) (2 429) (3 429) (4 429) (5 429) (6 429) (7 429)
          (8 429) (9 429)))))
      (429
       (Node
        (next_nodes
         ((0 430) (1 430) (2 430) (3 430) (4 430) (5 430) (6 430) (7 430)
          (8 430) (9 430)))))
      (430 (Node (next_nodes ((")" 431))))) (431 (Node (next_nodes ((" " 432)))))
      (432
       (Node
        (next_nodes
         ((0 433) (1 433) (2 433) (3 433) (4 433) (5 433) (6 433) (7 433)
          (8 433) (9 433)))))
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
      (435 (Node (next_nodes ((- 436)))))
      (436
       (Node
        (next_nodes
         ((0 437) (1 437) (2 437) (3 437) (4 437) (5 437) (6 437) (7 437)
          (8 437) (9 437)))))
      (437
       (Node
        (next_nodes
         ((0 438) (1 438) (2 438) (3 438) (4 438) (5 438) (6 438) (7 438)
          (8 438) (9 438)))))
      (438
       (Node
        (next_nodes
         ((0 439) (1 439) (2 439) (3 439) (4 439) (5 439) (6 439) (7 439)
          (8 439) (9 439)))))
      (439
       (Node
        (next_nodes
         ((0 440) (1 440) (2 440) (3 440) (4 440) (5 440) (6 440) (7 440)
          (8 440) (9 440)))))
      (440
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (441
       (Node
        (next_nodes
         ((0 442) (1 442) (2 442) (3 442) (4 442) (5 442) (6 442) (7 442)
          (8 442) (9 442)))))
      (442
       (Node
        (next_nodes
         ((" " 443) (0 442) (1 442) (2 442) (3 442) (4 442) (5 442) (6 442)
          (7 442) (8 442) (9 442)))))
      (443
       (Node
        (next_nodes
         (("(" 427) (0 444) (1 444) (2 444) (3 444) (4 444) (5 444) (6 444)
          (7 444) (8 444) (9 444)))))
      (444
       (Node
        (next_nodes
         ((0 445) (1 445) (2 445) (3 445) (4 445) (5 445) (6 445) (7 445)
          (8 445) (9 445)))))
      (445
       (Node
        (next_nodes
         ((0 446) (1 446) (2 446) (3 446) (4 446) (5 446) (6 446) (7 446)
          (8 446) (9 446)))))
      (446
       (Node
        (next_nodes
         ((- 432) (0 447) (1 447) (2 447) (3 447) (4 447) (5 447) (6 447)
          (7 447) (8 447) (9 447)))))
      (447
       (Node
        (next_nodes
         ((0 448) (1 448) (2 448) (3 448) (4 448) (5 448) (6 448) (7 448)
          (8 448) (9 448)))))
      (448
       (Node
        (next_nodes
         ((0 436) (1 436) (2 436) (3 436) (4 436) (5 436) (6 436) (7 436)
          (8 436) (9 436)))))))
    |}]
;;
