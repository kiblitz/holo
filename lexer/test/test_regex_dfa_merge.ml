open! Core
open! Import
open! Regex_dfa.For_testing

let merged_dfa =
  let to_dfa ~tag =
    let cont_of_match buffer = [%string "%{tag}:%{(Buffer.contents buffer)}"] in
    Regex_dfa.create ~cont_of_match
  in
  let in_ = Regex_config.exact "in" |> to_dfa ~tag:"in" in
  let int = Regex_config.exact "int" |> to_dfa ~tag:"int" in
  let id = to_dfa ~priority:0 Util.Common_config.identifier ~tag:"id" in
  Regex_dfa.merge_list Nonempty_list.[ in_; int; id ]
;;

let print_next_result ?(silent = false) iterator c =
  let result = Regex_dfa.Iterator.next iterator ~c in
  if not silent then print_s [%message (result : string Regex_dfa.Iterator.Result.t)]
;;

let%expect_test "merged dfa" =
  print_s [%message (merged_dfa : string t)];
  [%expect
    {|
    (merged_dfa
     ((408
       (Accept
        (next_nodes
         ((0 408) (1 408) (2 408) (3 408) (4 408) (5 408) (6 408) (7 408)
          (8 408) (9 408) (A 408) (B 408) (C 408) (D 408) (E 408) (F 408)
          (G 408) (H 408) (I 408) (J 408) (K 408) (L 408) (M 408) (N 408)
          (O 408) (P 408) (Q 408) (R 408) (S 408) (T 408) (U 408) (V 408)
          (W 408) (X 408) (Y 408) (Z 408) (_ 408) (a 408) (b 408) (c 408)
          (d 408) (e 408) (f 408) (g 408) (h 408) (i 408) (j 408) (k 408)
          (l 408) (m 408) (n 408) (o 408) (p 408) (q 408) (r 408) (s 408)
          (t 408) (u 408) (v 408) (w 408) (x 408) (y 408) (z 408)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (412
       (Node
        (next_nodes
         ((A 408) (B 408) (C 408) (D 408) (E 408) (F 408) (G 408) (H 408)
          (I 408) (J 408) (K 408) (L 408) (M 408) (N 408) (O 408) (P 408)
          (Q 408) (R 408) (S 408) (T 408) (U 408) (V 408) (W 408) (X 408)
          (Y 408) (Z 408) (_ 408) (a 408) (b 408) (c 408) (d 408) (e 408)
          (f 408) (g 408) (h 408) (i 413) (j 408) (k 408) (l 408) (m 408)
          (n 408) (o 408) (p 408) (q 408) (r 408) (s 408) (t 408) (u 408)
          (v 408) (w 408) (x 408) (y 408) (z 408)))))
      (413
       (Accept
        (next_nodes
         ((0 408) (1 408) (2 408) (3 408) (4 408) (5 408) (6 408) (7 408)
          (8 408) (9 408) (A 408) (B 408) (C 408) (D 408) (E 408) (F 408)
          (G 408) (H 408) (I 408) (J 408) (K 408) (L 408) (M 408) (N 408)
          (O 408) (P 408) (Q 408) (R 408) (S 408) (T 408) (U 408) (V 408)
          (W 408) (X 408) (Y 408) (Z 408) (_ 408) (a 408) (b 408) (c 408)
          (d 408) (e 408) (f 408) (g 408) (h 408) (i 408) (j 408) (k 408)
          (l 408) (m 408) (n 414) (o 408) (p 408) (q 408) (r 408) (s 408)
          (t 408) (u 408) (v 408) (w 408) (x 408) (y 408) (z 408)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (414
       (Accept
        (next_nodes
         ((0 408) (1 408) (2 408) (3 408) (4 408) (5 408) (6 408) (7 408)
          (8 408) (9 408) (A 408) (B 408) (C 408) (D 408) (E 408) (F 408)
          (G 408) (H 408) (I 408) (J 408) (K 408) (L 408) (M 408) (N 408)
          (O 408) (P 408) (Q 408) (R 408) (S 408) (T 408) (U 408) (V 408)
          (W 408) (X 408) (Y 408) (Z 408) (_ 408) (a 408) (b 408) (c 408)
          (d 408) (e 408) (f 408) (g 408) (h 408) (i 408) (j 408) (k 408)
          (l 408) (m 408) (n 408) (o 408) (p 408) (q 408) (r 408) (s 408)
          (t 415) (u 408) (v 408) (w 408) (x 408) (y 408) (z 408)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (415
       (Accept
        (next_nodes
         ((0 408) (1 408) (2 408) (3 408) (4 408) (5 408) (6 408) (7 408)
          (8 408) (9 408) (A 408) (B 408) (C 408) (D 408) (E 408) (F 408)
          (G 408) (H 408) (I 408) (J 408) (K 408) (L 408) (M 408) (N 408)
          (O 408) (P 408) (Q 408) (R 408) (S 408) (T 408) (U 408) (V 408)
          (W 408) (X 408) (Y 408) (Z 408) (_ 408) (a 408) (b 408) (c 408)
          (d 408) (e 408) (f 408) (g 408) (h 408) (i 408) (j 408) (k 408)
          (l 408) (m 408) (n 408) (o 408) (p 408) (q 408) (r 408) (s 408)
          (t 408) (u 408) (v 408) (w 408) (x 408) (y 408) (z 408)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "merged dfa iteration (success)" =
  let () =
    let id_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result id_iterator 'F';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator 'o';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator 'o';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator '2';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result id_iterator '\n';
    [%expect {| (result (Complete (result id:Foo2) (unused_len 1))) |}]
  in
  let () =
    let in_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result in_iterator 'i';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result in_iterator 'n';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result in_iterator '\n';
    [%expect {| (result (Complete (result in:in) (unused_len 1))) |}]
  in
  let () =
    let int_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result int_iterator 'i';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator 'n';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator 't';
    [%expect {| (result (Incomplete (is_accepting_state true))) |}];
    print_next_result int_iterator '\n';
    [%expect {| (result (Complete (result int:int) (unused_len 1))) |}]
  in
  ()
;;

let%expect_test "merged dfa iteration (failure)" =
  let iterator = Regex_dfa.Iterator.make merged_dfa in
  print_next_result iterator '3';
  [%expect {| (result (Failure (input 3))) |}]
;;
