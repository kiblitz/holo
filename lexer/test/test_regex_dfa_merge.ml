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
     ((405
       (Accept
        (next_nodes
         ((0 405) (1 405) (2 405) (3 405) (4 405) (5 405) (6 405) (7 405)
          (8 405) (9 405) (A 405) (B 405) (C 405) (D 405) (E 405) (F 405)
          (G 405) (H 405) (I 405) (J 405) (K 405) (L 405) (M 405) (N 405)
          (O 405) (P 405) (Q 405) (R 405) (S 405) (T 405) (U 405) (V 405)
          (W 405) (X 405) (Y 405) (Z 405) (_ 405) (a 405) (b 405) (c 405)
          (d 405) (e 405) (f 405) (g 405) (h 405) (i 405) (j 405) (k 405)
          (l 405) (m 405) (n 405) (o 405) (p 405) (q 405) (r 405) (s 405)
          (t 405) (u 405) (v 405) (w 405) (x 405) (y 405) (z 405)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (409
       (Node
        (next_nodes
         ((A 405) (B 405) (C 405) (D 405) (E 405) (F 405) (G 405) (H 405)
          (I 405) (J 405) (K 405) (L 405) (M 405) (N 405) (O 405) (P 405)
          (Q 405) (R 405) (S 405) (T 405) (U 405) (V 405) (W 405) (X 405)
          (Y 405) (Z 405) (_ 405) (a 405) (b 405) (c 405) (d 405) (e 405)
          (f 405) (g 405) (h 405) (i 410) (j 405) (k 405) (l 405) (m 405)
          (n 405) (o 405) (p 405) (q 405) (r 405) (s 405) (t 405) (u 405)
          (v 405) (w 405) (x 405) (y 405) (z 405)))))
      (410
       (Accept
        (next_nodes
         ((0 405) (1 405) (2 405) (3 405) (4 405) (5 405) (6 405) (7 405)
          (8 405) (9 405) (A 405) (B 405) (C 405) (D 405) (E 405) (F 405)
          (G 405) (H 405) (I 405) (J 405) (K 405) (L 405) (M 405) (N 405)
          (O 405) (P 405) (Q 405) (R 405) (S 405) (T 405) (U 405) (V 405)
          (W 405) (X 405) (Y 405) (Z 405) (_ 405) (a 405) (b 405) (c 405)
          (d 405) (e 405) (f 405) (g 405) (h 405) (i 405) (j 405) (k 405)
          (l 405) (m 405) (n 411) (o 405) (p 405) (q 405) (r 405) (s 405)
          (t 405) (u 405) (v 405) (w 405) (x 405) (y 405) (z 405)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (411
       (Accept
        (next_nodes
         ((0 405) (1 405) (2 405) (3 405) (4 405) (5 405) (6 405) (7 405)
          (8 405) (9 405) (A 405) (B 405) (C 405) (D 405) (E 405) (F 405)
          (G 405) (H 405) (I 405) (J 405) (K 405) (L 405) (M 405) (N 405)
          (O 405) (P 405) (Q 405) (R 405) (S 405) (T 405) (U 405) (V 405)
          (W 405) (X 405) (Y 405) (Z 405) (_ 405) (a 405) (b 405) (c 405)
          (d 405) (e 405) (f 405) (g 405) (h 405) (i 405) (j 405) (k 405)
          (l 405) (m 405) (n 405) (o 405) (p 405) (q 405) (r 405) (s 405)
          (t 412) (u 405) (v 405) (w 405) (x 405) (y 405) (z 405)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (412
       (Accept
        (next_nodes
         ((0 405) (1 405) (2 405) (3 405) (4 405) (5 405) (6 405) (7 405)
          (8 405) (9 405) (A 405) (B 405) (C 405) (D 405) (E 405) (F 405)
          (G 405) (H 405) (I 405) (J 405) (K 405) (L 405) (M 405) (N 405)
          (O 405) (P 405) (Q 405) (R 405) (S 405) (T 405) (U 405) (V 405)
          (W 405) (X 405) (Y 405) (Z 405) (_ 405) (a 405) (b 405) (c 405)
          (d 405) (e 405) (f 405) (g 405) (h 405) (i 405) (j 405) (k 405)
          (l 405) (m 405) (n 405) (o 405) (p 405) (q 405) (r 405) (s 405)
          (t 405) (u 405) (v 405) (w 405) (x 405) (y 405) (z 405)))
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
