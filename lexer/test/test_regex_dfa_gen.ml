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
     ((404 (Node (next_nodes ((f 405))))) (405 (Node (next_nodes ((o 406)))))
      (406 (Node (next_nodes ((o 407))))) (407 (Node (next_nodes ((" " 408)))))
      (408
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
     ((409 (Node (next_nodes ((a 410) (b 410) (r 410)))))
      (410
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
     ((411
       (Node
        (next_nodes
         ((0 412) (1 412) (2 412) (3 412) (4 412) (5 412) (6 412) (7 412)
          (8 412) (9 412)))))
      (412
       (Node
        (next_nodes
         ((. 413) (0 412) (1 412) (2 412) (3 412) (4 412) (5 412) (6 412)
          (7 412) (8 412) (9 412)))))
      (413
       (Accept
        (next_nodes
         ((0 413) (1 413) (2 413) (3 413) (4 413) (5 413) (6 413) (7 413)
          (8 413) (9 413)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "complex dfa: phone" =
  let dfa = to_dfa Util.Common_config.phone in
  print_s [%message (dfa : unit t)];
  [%expect
    {|
    (dfa
     ((414
       (Node
        (next_nodes
         (("(" 415) (+ 429) (0 432) (1 432) (2 432) (3 432) (4 432) (5 432)
          (6 432) (7 432) (8 432) (9 432)))))
      (415
       (Node
        (next_nodes
         ((0 416) (1 416) (2 416) (3 416) (4 416) (5 416) (6 416) (7 416)
          (8 416) (9 416)))))
      (416
       (Node
        (next_nodes
         ((0 417) (1 417) (2 417) (3 417) (4 417) (5 417) (6 417) (7 417)
          (8 417) (9 417)))))
      (417
       (Node
        (next_nodes
         ((0 418) (1 418) (2 418) (3 418) (4 418) (5 418) (6 418) (7 418)
          (8 418) (9 418)))))
      (418 (Node (next_nodes ((")" 419))))) (419 (Node (next_nodes ((" " 420)))))
      (420
       (Node
        (next_nodes
         ((0 421) (1 421) (2 421) (3 421) (4 421) (5 421) (6 421) (7 421)
          (8 421) (9 421)))))
      (421
       (Node
        (next_nodes
         ((0 422) (1 422) (2 422) (3 422) (4 422) (5 422) (6 422) (7 422)
          (8 422) (9 422)))))
      (422
       (Node
        (next_nodes
         ((0 423) (1 423) (2 423) (3 423) (4 423) (5 423) (6 423) (7 423)
          (8 423) (9 423)))))
      (423 (Node (next_nodes ((- 424)))))
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
      (427
       (Node
        (next_nodes
         ((0 428) (1 428) (2 428) (3 428) (4 428) (5 428) (6 428) (7 428)
          (8 428) (9 428)))))
      (428
       (Accept (next_nodes ())
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (429
       (Node
        (next_nodes
         ((0 430) (1 430) (2 430) (3 430) (4 430) (5 430) (6 430) (7 430)
          (8 430) (9 430)))))
      (430
       (Node
        (next_nodes
         ((" " 431) (0 430) (1 430) (2 430) (3 430) (4 430) (5 430) (6 430)
          (7 430) (8 430) (9 430)))))
      (431
       (Node
        (next_nodes
         (("(" 415) (0 432) (1 432) (2 432) (3 432) (4 432) (5 432) (6 432)
          (7 432) (8 432) (9 432)))))
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
         ((- 420) (0 435) (1 435) (2 435) (3 435) (4 435) (5 435) (6 435)
          (7 435) (8 435) (9 435)))))
      (435
       (Node
        (next_nodes
         ((0 436) (1 436) (2 436) (3 436) (4 436) (5 436) (6 436) (7 436)
          (8 436) (9 436)))))
      (436
       (Node
        (next_nodes
         ((0 424) (1 424) (2 424) (3 424) (4 424) (5 424) (6 424) (7 424)
          (8 424) (9 424)))))))
    |}]
;;
