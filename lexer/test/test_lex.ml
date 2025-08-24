open! Core
open! Import

let%expect_test "constants" =
  let tokens =
    Lexer.lex
      {|
int = 5 in
bool = true in
float = 2.71 in
char = '\n' in
string = "hello world" in
();
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Identifier ((name int))) (Symbol (Base (Equal))) (Constant (Int 5))
        (Definition In) (Identifier ((name bool))) (Symbol (Base (Equal)))
        (Constant (Bool true)) (Definition In) (Identifier ((name float)))
        (Symbol (Base (Equal))) (Constant (Float 2.71)) (Definition In)
        (Identifier ((name char))) (Symbol (Base (Equal)))
        (Constant (Char "\\n")) (Definition In) (Identifier ((name string)))
        (Symbol (Base (Equal))) (Constant (String "hello world")) (Definition In)
        (Grouping (Parenthesis Left)) (Grouping (Parenthesis Right))
        (Symbol (Non_custom Semicolon))))
      (errors ())))
    |}]
;;

let%expect_test "symbols" =
  let tokens =
    Lexer.lex
      {|
foo = ((a * b)[c] // d |> e f) && (g ** h ^ i @ j) || ($k <~> l%) in
bar = (m, n) := [o = p; q : r.s] in
();
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Identifier ((name foo))) (Symbol (Base (Equal)))
        (Grouping (Parenthesis Left)) (Grouping (Parenthesis Left))
        (Identifier ((name a))) (Symbol (Base (Times))) (Identifier ((name b)))
        (Grouping (Parenthesis Right)) (Grouping (Square_bracket Left))
        (Identifier ((name c))) (Grouping (Square_bracket Right))
        (Symbol (Base (Div Div))) (Identifier ((name d)))
        (Symbol (Base (Pipe Greater))) (Identifier ((name e)))
        (Identifier ((name f))) (Grouping (Parenthesis Right))
        (Symbol (Base (Ampersand Ampersand))) (Grouping (Parenthesis Left))
        (Identifier ((name g))) (Symbol (Base (Times Times)))
        (Identifier ((name h))) (Symbol (Base (Caret))) (Identifier ((name i)))
        (Symbol (Base (At))) (Identifier ((name j)))
        (Grouping (Parenthesis Right)) (Symbol (Base (Pipe Pipe)))
        (Grouping (Parenthesis Left)) (Symbol (Base (Dollar)))
        (Identifier ((name k))) (Symbol (Base (Less Tilda Greater)))
        (Identifier ((name l))) (Symbol (Base (Percent)))
        (Grouping (Parenthesis Right)) (Definition In) (Identifier ((name bar)))
        (Symbol (Base (Equal))) (Grouping (Parenthesis Left))
        (Identifier ((name m))) (Symbol (Non_custom Comma))
        (Identifier ((name n))) (Grouping (Parenthesis Right))
        (Symbol (Non_custom Walrus)) (Grouping (Square_bracket Left))
        (Identifier ((name o))) (Symbol (Base (Equal))) (Identifier ((name p)))
        (Symbol (Non_custom Semicolon)) (Identifier ((name q)))
        (Symbol (Non_custom Colon)) (Identifier ((name r)))
        (Symbol (Non_custom Dot)) (Identifier ((name s)))
        (Grouping (Square_bracket Right)) (Definition In)
        (Grouping (Parenthesis Left)) (Grouping (Parenthesis Right))
        (Symbol (Non_custom Semicolon))))
      (errors ())))
    |}]
;;

let%expect_test "import" =
  let tokens =
    Lexer.lex
      {|
open! Stdlib
open Async
include Import
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Import (Open (allow_unused true))) (Identifier ((name Stdlib)))
        (Import (Open (allow_unused false))) (Identifier ((name Async)))
        (Import Include) (Identifier ((name Import)))))
      (errors ())))
    |}]
;;

let%expect_test "definition" =
  let tokens =
    Lexer.lex
      {|
module#monad T : sig
   foo : ~a -> b -> c;
   bar : unit -> unit;
end = struct
   rec foo ~a b = 
      #bind a and b in
      a + b
   and bar () = ();
end
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Definition (Module (((name monad))))) (Identifier ((name T)))
        (Symbol (Non_custom Colon)) (Definition Sig) (Identifier ((name foo)))
        (Symbol (Non_custom Colon)) (Symbol (Base (Tilda)))
        (Identifier ((name a))) (Symbol (Base (Minus Greater)))
        (Identifier ((name b))) (Symbol (Base (Minus Greater)))
        (Identifier ((name c))) (Symbol (Non_custom Semicolon))
        (Identifier ((name bar))) (Symbol (Non_custom Colon))
        (Identifier ((name unit))) (Symbol (Base (Minus Greater)))
        (Identifier ((name unit))) (Symbol (Non_custom Semicolon))
        (Definition End) (Symbol (Base (Equal))) (Definition Struct)
        (Definition Rec) (Identifier ((name foo))) (Symbol (Base (Tilda)))
        (Identifier ((name a))) (Identifier ((name b))) (Symbol (Base (Equal)))
        (Definition (Assign_with_transformation ((name bind))))
        (Identifier ((name a))) (Definition And) (Identifier ((name b)))
        (Definition In) (Identifier ((name a))) (Symbol (Base (Plus)))
        (Identifier ((name b))) (Definition And) (Identifier ((name bar)))
        (Grouping (Parenthesis Left)) (Grouping (Parenthesis Right))
        (Symbol (Base (Equal))) (Grouping (Parenthesis Left))
        (Grouping (Parenthesis Right)) (Symbol (Non_custom Semicolon))
        (Definition End)))
      (errors ())))
    |}]
;;

let%expect_test "conditional" =
  let tokens =
    Lexer.lex
      {|
x = match foo:
    | Some bar when bar: 10
    | Some bar: 22
    | None: if true then 0 else 1 in
y = function
    | _ -> x;
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Identifier ((name x))) (Symbol (Base (Equal))) (Conditional (Match ()))
        (Identifier ((name foo))) (Symbol (Non_custom Colon))
        (Symbol (Base (Pipe))) (Identifier ((name Some)))
        (Identifier ((name bar))) (Conditional When) (Identifier ((name bar)))
        (Symbol (Non_custom Colon)) (Constant (Int 10)) (Symbol (Base (Pipe)))
        (Identifier ((name Some))) (Identifier ((name bar)))
        (Symbol (Non_custom Colon)) (Constant (Int 22)) (Symbol (Base (Pipe)))
        (Identifier ((name None))) (Symbol (Non_custom Colon))
        (Conditional (If ())) (Constant (Bool true)) (Conditional Then)
        (Constant (Int 0)) (Conditional Else) (Constant (Int 1)) (Definition In)
        (Identifier ((name y))) (Symbol (Base (Equal))) (Conditional Function)
        (Symbol (Base (Pipe))) (Identifier ((name _)))
        (Symbol (Base (Minus Greater))) (Identifier ((name x)))
        (Symbol (Non_custom Semicolon))))
      (errors ())))
    |}]
;;

let%expect_test "typeful" =
  let tokens =
    Lexer.lex
      {|
type t = Foo of int
type nonrec t2 = { mutable foo : int }
x ({ foo } as bar) = foo;
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Typeful Type) (Identifier ((name t))) (Symbol (Base (Equal)))
        (Identifier ((name Foo))) (Typeful Of) (Identifier ((name int)))
        (Typeful Type) (Typeful Nonrec) (Identifier ((name t2)))
        (Symbol (Base (Equal))) (Grouping (Curly_bracket Left)) (Typeful Mutable)
        (Identifier ((name foo))) (Symbol (Non_custom Colon))
        (Identifier ((name int))) (Grouping (Curly_bracket Right))
        (Identifier ((name x))) (Grouping (Parenthesis Left))
        (Grouping (Curly_bracket Left)) (Identifier ((name foo)))
        (Grouping (Curly_bracket Right)) (Typeful As) (Identifier ((name bar)))
        (Grouping (Parenthesis Right)) (Symbol (Base (Equal)))
        (Identifier ((name foo))) (Symbol (Non_custom Semicolon))))
      (errors ())))
    |}]
;;

let%expect_test "misc" =
  let tokens =
    Lexer.lex
      {|
x = lambda a b c: a + b + c;

include functor Make with type t := t
|}
  in
  print_s [%message (tokens : Token.t list With_errors.t)];
  [%expect
    {|
    (tokens
     ((value
       ((Identifier ((name x))) (Symbol (Base (Equal))) Lambda
        (Identifier ((name a))) (Identifier ((name b))) (Identifier ((name c)))
        (Symbol (Non_custom Colon)) (Identifier ((name a)))
        (Symbol (Base (Plus))) (Identifier ((name b))) (Symbol (Base (Plus)))
        (Identifier ((name c))) (Symbol (Non_custom Semicolon)) (Import Include)
        Functor (Identifier ((name Make))) With (Typeful Type)
        (Identifier ((name t))) (Symbol (Non_custom Walrus))
        (Identifier ((name t)))))
      (errors ())))
    |}]
;;
