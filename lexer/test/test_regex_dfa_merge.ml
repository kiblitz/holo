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
     ((396
       (Accept
        (next_nodes
         ((0 396) (1 396) (2 396) (3 396) (4 396) (5 396) (6 396) (7 396)
          (8 396) (9 396) (A 396) (B 396) (C 396) (D 396) (E 396) (F 396)
          (G 396) (H 396) (I 396) (J 396) (K 396) (L 396) (M 396) (N 396)
          (O 396) (P 396) (Q 396) (R 396) (S 396) (T 396) (U 396) (V 396)
          (W 396) (X 396) (Y 396) (Z 396) (_ 396) (a 396) (b 396) (c 396)
          (d 396) (e 396) (f 396) (g 396) (h 396) (i 396) (j 396) (k 396)
          (l 396) (m 396) (n 396) (o 396) (p 396) (q 396) (r 396) (s 396)
          (t 396) (u 396) (v 396) (w 396) (x 396) (y 396) (z 396)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (400
       (Node
        (next_nodes
         ((A 396) (B 396) (C 396) (D 396) (E 396) (F 396) (G 396) (H 396)
          (I 396) (J 396) (K 396) (L 396) (M 396) (N 396) (O 396) (P 396)
          (Q 396) (R 396) (S 396) (T 396) (U 396) (V 396) (W 396) (X 396)
          (Y 396) (Z 396) (_ 396) (a 396) (b 396) (c 396) (d 396) (e 396)
          (f 396) (g 396) (h 396) (i 401) (j 396) (k 396) (l 396) (m 396)
          (n 396) (o 396) (p 396) (q 396) (r 396) (s 396) (t 396) (u 396)
          (v 396) (w 396) (x 396) (y 396) (z 396)))))
      (401
       (Accept
        (next_nodes
         ((0 396) (1 396) (2 396) (3 396) (4 396) (5 396) (6 396) (7 396)
          (8 396) (9 396) (A 396) (B 396) (C 396) (D 396) (E 396) (F 396)
          (G 396) (H 396) (I 396) (J 396) (K 396) (L 396) (M 396) (N 396)
          (O 396) (P 396) (Q 396) (R 396) (S 396) (T 396) (U 396) (V 396)
          (W 396) (X 396) (Y 396) (Z 396) (_ 396) (a 396) (b 396) (c 396)
          (d 396) (e 396) (f 396) (g 396) (h 396) (i 396) (j 396) (k 396)
          (l 396) (m 396) (n 402) (o 396) (p 396) (q 396) (r 396) (s 396)
          (t 396) (u 396) (v 396) (w 396) (x 396) (y 396) (z 396)))
        (accepting_state_metadata ((priority 0) (cont_of_match <fun>)))))
      (402
       (Accept
        (next_nodes
         ((0 396) (1 396) (2 396) (3 396) (4 396) (5 396) (6 396) (7 396)
          (8 396) (9 396) (A 396) (B 396) (C 396) (D 396) (E 396) (F 396)
          (G 396) (H 396) (I 396) (J 396) (K 396) (L 396) (M 396) (N 396)
          (O 396) (P 396) (Q 396) (R 396) (S 396) (T 396) (U 396) (V 396)
          (W 396) (X 396) (Y 396) (Z 396) (_ 396) (a 396) (b 396) (c 396)
          (d 396) (e 396) (f 396) (g 396) (h 396) (i 396) (j 396) (k 396)
          (l 396) (m 396) (n 396) (o 396) (p 396) (q 396) (r 396) (s 396)
          (t 403) (u 396) (v 396) (w 396) (x 396) (y 396) (z 396)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))
      (403
       (Accept
        (next_nodes
         ((0 396) (1 396) (2 396) (3 396) (4 396) (5 396) (6 396) (7 396)
          (8 396) (9 396) (A 396) (B 396) (C 396) (D 396) (E 396) (F 396)
          (G 396) (H 396) (I 396) (J 396) (K 396) (L 396) (M 396) (N 396)
          (O 396) (P 396) (Q 396) (R 396) (S 396) (T 396) (U 396) (V 396)
          (W 396) (X 396) (Y 396) (Z 396) (_ 396) (a 396) (b 396) (c 396)
          (d 396) (e 396) (f 396) (g 396) (h 396) (i 396) (j 396) (k 396)
          (l 396) (m 396) (n 396) (o 396) (p 396) (q 396) (r 396) (s 396)
          (t 396) (u 396) (v 396) (w 396) (x 396) (y 396) (z 396)))
        (accepting_state_metadata ((priority 1) (cont_of_match <fun>)))))))
    |}]
;;

let%expect_test "merged dfa iteration (success)" =
  let () =
    let id_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result id_iterator 'F';
    [%expect {| (result Incomplete) |}];
    print_next_result id_iterator 'o';
    [%expect {| (result Incomplete) |}];
    print_next_result id_iterator 'o';
    [%expect {| (result Incomplete) |}];
    print_next_result id_iterator '2';
    [%expect {| (result Incomplete) |}];
    print_next_result id_iterator '\n';
    [%expect {| (result (Complete (result id:Foo2) (unused "\n"))) |}]
  in
  let () =
    let in_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result in_iterator 'i';
    [%expect {| (result Incomplete) |}];
    print_next_result in_iterator 'n';
    [%expect {| (result Incomplete) |}];
    print_next_result in_iterator '\n';
    [%expect {| (result (Complete (result in:in) (unused "\n"))) |}]
  in
  let () =
    let int_iterator = Regex_dfa.Iterator.make merged_dfa in
    print_next_result int_iterator 'i';
    [%expect {| (result Incomplete) |}];
    print_next_result int_iterator 'n';
    [%expect {| (result Incomplete) |}];
    print_next_result int_iterator 't';
    [%expect {| (result Incomplete) |}];
    print_next_result int_iterator '\n';
    [%expect {| (result (Complete (result int:int) (unused "\n"))) |}]
  in
  ()
;;

let%expect_test "merged dfa iteration (failure)" =
  let iterator = Regex_dfa.Iterator.make merged_dfa in
  print_next_result iterator '3';
  [%expect {| (result (Failure (input 3))) |}]
;;
