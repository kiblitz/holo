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
     ((404
       (Accept
        (next_nodes
         ((0 404) (1 404) (2 404) (3 404) (4 404) (5 404) (6 404) (7 404)
          (8 404) (9 404) (A 404) (B 404) (C 404) (D 404) (E 404) (F 404)
          (G 404) (H 404) (I 404) (J 404) (K 404) (L 404) (M 404) (N 404)
          (O 404) (P 404) (Q 404) (R 404) (S 404) (T 404) (U 404) (V 404)
          (W 404) (X 404) (Y 404) (Z 404) (_ 404) (a 404) (b 404) (c 404)
          (d 404) (e 404) (f 404) (g 404) (h 404) (i 404) (j 404) (k 404)
          (l 404) (m 404) (n 404) (o 404) (p 404) (q 404) (r 404) (s 404)
          (t 404) (u 404) (v 404) (w 404) (x 404) (y 404) (z 404)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (408
       (Node
        (next_nodes
         ((A 404) (B 404) (C 404) (D 404) (E 404) (F 404) (G 404) (H 404)
          (I 404) (J 404) (K 404) (L 404) (M 404) (N 404) (O 404) (P 404)
          (Q 404) (R 404) (S 404) (T 404) (U 404) (V 404) (W 404) (X 404)
          (Y 404) (Z 404) (_ 404) (a 404) (b 404) (c 404) (d 404) (e 404)
          (f 404) (g 404) (h 404) (i 409) (j 404) (k 404) (l 404) (m 404)
          (n 404) (o 404) (p 404) (q 404) (r 404) (s 404) (t 404) (u 404)
          (v 404) (w 404) (x 404) (y 404) (z 404)))))
      (409
       (Accept
        (next_nodes
         ((0 404) (1 404) (2 404) (3 404) (4 404) (5 404) (6 404) (7 404)
          (8 404) (9 404) (A 404) (B 404) (C 404) (D 404) (E 404) (F 404)
          (G 404) (H 404) (I 404) (J 404) (K 404) (L 404) (M 404) (N 404)
          (O 404) (P 404) (Q 404) (R 404) (S 404) (T 404) (U 404) (V 404)
          (W 404) (X 404) (Y 404) (Z 404) (_ 404) (a 404) (b 404) (c 404)
          (d 404) (e 404) (f 404) (g 404) (h 404) (i 404) (j 404) (k 404)
          (l 404) (m 404) (n 410) (o 404) (p 404) (q 404) (r 404) (s 404)
          (t 404) (u 404) (v 404) (w 404) (x 404) (y 404) (z 404)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (410
       (Accept
        (next_nodes
         ((0 404) (1 404) (2 404) (3 404) (4 404) (5 404) (6 404) (7 404)
          (8 404) (9 404) (A 404) (B 404) (C 404) (D 404) (E 404) (F 404)
          (G 404) (H 404) (I 404) (J 404) (K 404) (L 404) (M 404) (N 404)
          (O 404) (P 404) (Q 404) (R 404) (S 404) (T 404) (U 404) (V 404)
          (W 404) (X 404) (Y 404) (Z 404) (_ 404) (a 404) (b 404) (c 404)
          (d 404) (e 404) (f 404) (g 404) (h 404) (i 404) (j 404) (k 404)
          (l 404) (m 404) (n 404) (o 404) (p 404) (q 404) (r 404) (s 404)
          (t 411) (u 404) (v 404) (w 404) (x 404) (y 404) (z 404)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (411
       (Accept
        (next_nodes
         ((0 404) (1 404) (2 404) (3 404) (4 404) (5 404) (6 404) (7 404)
          (8 404) (9 404) (A 404) (B 404) (C 404) (D 404) (E 404) (F 404)
          (G 404) (H 404) (I 404) (J 404) (K 404) (L 404) (M 404) (N 404)
          (O 404) (P 404) (Q 404) (R 404) (S 404) (T 404) (U 404) (V 404)
          (W 404) (X 404) (Y 404) (Z 404) (_ 404) (a 404) (b 404) (c 404)
          (d 404) (e 404) (f 404) (g 404) (h 404) (i 404) (j 404) (k 404)
          (l 404) (m 404) (n 404) (o 404) (p 404) (q 404) (r 404) (s 404)
          (t 404) (u 404) (v 404) (w 404) (x 404) (y 404) (z 404)))
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
